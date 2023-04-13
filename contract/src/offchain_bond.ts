import {
  Address,
  AddressDetails,
  Assets,
  Credential,
  Data,
  Datum,
  fromText,
  fromUnit,
  Lovelace,
  Lucid,
  OutRef,
  paymentCredentialOf,
  PolicyId,
  ScriptHash,
  toUnit,
  Tx,
  TxHash,
  UTxO,
} from "../../deps.ts";
import {
  fromAddress,
  fromAssets,
  sortAsc,
  sortDesc,
  toAddress,
  toAssets,
} from "../../common/utils.ts";
import * as D from "../../common/contract.types.ts";
import {
  AssetName,
  BondInfo,
  CadogoConfig,
  Constraints,
  NameAndQuantity,
} from "./types.ts";

export class ContractBond {
  lucid: Lucid;
  config: CadogoConfig;

  constructor(
    lucid: Lucid,
    config: CadogoConfig,
  ) {
    this.lucid = lucid;
    this.config = config;
  }

  async buy(listingUtxos: { utxo: UTxO; qty: number }[]): Promise<TxHash> {
    const buyOrders = (await Promise.all(
      listingUtxos.map((listingUtxo) =>
        this._buy(listingUtxo.utxo, BigInt(listingUtxo.qty))
      ),
    ))
      .reduce(
        (prevTx, tx) => prevTx.compose(tx),
        this.lucid.newTx(),
      );

    const tx = await this.lucid.newTx()
      .compose(buyOrders)
      .complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  /**
   * Accept specific bids.
   * Optionally you can accept open bids that demand any NFT from the collection for a certain lovelace amount.
   * Specify in this case the asset you are willing to sell for this price.
   */
  async sell(
    sellOptions: { bidUtxo: UTxO; assetName?: string }[],
  ): Promise<TxHash> {
    const sellOrders = (await Promise.all(
      sellOptions.map(({ bidUtxo, assetName }) =>
        this._sell(bidUtxo, assetName)
      ),
    ))
      .reduce(
        (prevTx, tx) => prevTx.compose(tx),
        this.lucid.newTx(),
      );

    const tx = await this.lucid.newTx()
      .compose(sellOrders)
      .complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  /**
   * List asset(s) for a specified lovelace value. Optionally the listing could be private.\
   * Assets can be specified as either an array of asset names
   * (assuming each asset has a quantity of 1) or as a map,
   * where the quantity of each asset can be chosen.
   */
  async list(
    assetName: AssetName,
    quantity: number,
    requestedYield: number,
  ): Promise<TxHash> {
    const tx = await this.lucid.newTx()
      .compose(
        await this._list({ [assetName]: BigInt(quantity) }, requestedYield),
      )
      .complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  async changeListing(
    listingUtxo: UTxO,
    _assetName: AssetName,
    quantity: number,
    requestedYield: number | undefined,
  ): Promise<TxHash> {
    // if (!(requestedYield > 1 && requestedYield < 9999)) {
    //   throw new Error("requested yield must be in range 1-9999");
    // }
    const listingDatum = await this.lucid.datumOf<D.CadogoBondListingDatum>(
      listingUtxo,
      D.CadogoBondListingDatum,
    );
    if (
      !("ownerPaymentKey" in listingDatum && "requestedYield" in listingDatum)
    ) {
      throw new Error("Not a listing UTxO");
    }

    const listingDatumOwner: D.Address = {
      paymentCredential: {
        PublicKeyCredential: [listingDatum.ownerPaymentKey],
      },
      stakeCredential: listingDatum.ownerStakeKey
        ? {
          Inline: [
            {
              PublicKeyCredential: [listingDatum.ownerStakeKey],
            },
          ],
        }
        : null,
    };
    const owner: Address = toAddress(listingDatumOwner, this.lucid);
    const ownerKey = paymentCredentialOf(owner).hash;
    const address: Address = await this.lucid.wallet.address();
    if (ownerKey !== paymentCredentialOf(address).hash) {
      throw new Error("You are not the owner.");
    }

    const listingToken = Object.keys(listingUtxo.assets).find((unit) =>
      unit.startsWith(this.config.bond.bondPolicyId)
    );
    if (!listingToken) throw new Error("No listing token found.");
    if (
      listingUtxo.assets[listingToken] == BigInt(quantity) &&
      requestedYield == undefined
    ) {
      throw new Error("Listing unnecessary update.");
    }

    if (
      requestedYield != undefined &&
      listingDatum.requestedYield != BigInt(requestedYield)
    ) {
      listingDatum.requestedYield = BigInt(requestedYield);
    }

    const refScripts = await this.getDeployedScripts();
    const redeemer = Data.to<D.CadogoBondTradeAction>(
      "Update",
      D.CadogoBondTradeAction,
    );
    const tx = await this.lucid.newTx()
      .collectFrom(
        [listingUtxo],
        redeemer,
      )
      .payToContract(listingUtxo.address, {
        inline: Data.to<D.CadogoBondListingDatum>(
          listingDatum,
          D.CadogoBondListingDatum,
        ),
      }, Object.fromEntries([[listingToken, BigInt(quantity)]]))
      .addSignerKey(ownerKey)
      .compose(
        refScripts.trade
          ? this.lucid.newTx().readFrom([refScripts.trade])
          : this.lucid.newTx().attachSpendingValidator(
            this.config.tradeValidator,
          ),
      )
      .complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  /**
   * A bid can be placed on a specific token or a bundle within a collection
   * by specifying the assets as either an array of asset names
   * (assuming each asset has a quantity of 1) or as a map,
   * where the quantity of each asset can be chosen.
   */
  async bid(
    assets: NameAndQuantity | AssetName[],
    lovelace: Lovelace,
  ): Promise<TxHash> {
    const tx = await this.lucid.newTx()
      .compose(await this._bid(assets, lovelace))
      .complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  /** Create a collection offer on the collection. Optionally add constraints. */
  async bidOpen(
    lovelace: Lovelace,
    constraints?: {
      types?: string[];
      traits?: { negation?: boolean; trait: string }[];
    },
  ): Promise<TxHash> {
    const tx = await this.lucid.newTx()
      .compose(await this._bidOpen(lovelace, constraints))
      .complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  /** Swap asset(s) for other asset(s). Lovelace could also be included on the offering side. */
  async bidSwap(
    offering: {
      lovelace?: Lovelace;
      assetNames: string[];
    },
    requesting: {
      constraints?: Constraints;
      specific?: string[];
    },
  ): Promise<TxHash> {
    const tx = await this.lucid.newTx()
      .compose(await this._bidSwap(offering, requesting))
      .complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  async changeBid(bidUtxo: UTxO, lovelace: Lovelace): Promise<TxHash> {
    const tradeDatum = await this.lucid.datumOf<D.TradeDatum>(
      bidUtxo,
      D.TradeDatum,
    );
    if (!("Bid" in tradeDatum)) {
      throw new Error("Not a bidding UTxO");
    }

    if (Object.keys(bidUtxo.assets).length > 2) {
      throw new Error("Cannot change swap bids.");
    }

    const owner: Address = toAddress(tradeDatum.Bid[0].owner, this.lucid);
    const ownerKey = paymentCredentialOf(owner).hash;

    const address: Address = await this.lucid.wallet.address();

    if (ownerKey !== paymentCredentialOf(address).hash) {
      throw new Error("You are not the owner.");
    }

    const refScripts = await this.getDeployedScripts();

    const tx = await this.lucid.newTx().collectFrom(
      [bidUtxo],
      Data.to<D.CadogoBondTradeAction>("Update", D.CadogoBondTradeAction),
    ).payToContract(bidUtxo.address, {
      inline: bidUtxo.datum!,
    }, { ...bidUtxo.assets, lovelace })
      .addSignerKey(ownerKey)
      .compose(
        refScripts.trade
          ? this.lucid.newTx().readFrom([refScripts.trade])
          : this.lucid.newTx().attachSpendingValidator(
            this.config.tradeValidator,
          ),
      )
      .complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  async cancelListing(listingUtxo: UTxO): Promise<TxHash> {
    const tx = await this.lucid.newTx().compose(
      await this._cancelListing(listingUtxo),
    )
      .complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  async cancelBid(bidUtxo: UTxO): Promise<TxHash> {
    const tx = await this.lucid.newTx().compose(await this._cancelBid(bidUtxo))
      .complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  async cancelListingAndSell(
    listingUtxo: UTxO,
    bidUtxo: UTxO,
    assetName?: string,
  ): Promise<TxHash> {
    const tx = await this.lucid.newTx()
      .compose(await this._cancelListing(listingUtxo))
      .compose(await this._sell(bidUtxo, assetName))
      .complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  async cancelBidAndBuy(
    bidUtxo: UTxO,
    listingUtxo: UTxO,
  ): Promise<TxHash> {
    const tx = await this.lucid.newTx()
      .compose(await this._cancelBid(bidUtxo))
      .compose(await this._buy(listingUtxo, 1n))
      .complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  /** Get all listings and bids. If there are a lot of UTxOs it is recommended using an indexer (Nebula Watcher) instead. */
  async getAllListingsAndBids(): Promise<UTxO[]> {
    const utxos = await this.lucid.utxosAt(
      paymentCredentialOf(this.config.tradeAddress),
    );
    return utxos.filter((utxo: UTxO) =>
      Object.keys(utxo.assets)
        .filter((unit) => unit !== "lovelace")
        .every(
          (unit) =>
            unit.startsWith(this.config.mintPolicyId) ||
            unit.startsWith(this.config.bond.bondPolicyId),
        )
    );
  }

  /** Get a specific listing or bid. */
  async getListingOrBid(outRef: OutRef): Promise<UTxO | null> {
    const [utxo] = await this.lucid.utxosByOutRef([outRef]);
    return utxo || null;
  }

  /** Return the current listings for a specific asset sorted in ascending order by price. */
  async getListings(assetName: string): Promise<UTxO[]> {
    return (await this.lucid.utxosAtWithUnit(
      paymentCredentialOf(this.config.tradeAddress),
      toUnit(
        this.config.bond.bondPolicyId,
        assetName,
      ),
    )).filter((utxo: UTxO) => {
      const units = Object.keys(utxo.assets).filter((unit) =>
        unit !== "lovelace"
      );
      return units.every((unit) =>
        unit.startsWith(this.config.bond.bondPolicyId)
      ) &&
        units.length >= 1;
    }).sort(sortAsc);
  }

  async getBondInfos(assetName: string): Promise<UTxO[]> {
    return (await this.lucid.utxosByUnit(
      toUnit(
        this.config.bond.escrowPolicyId,
        assetName,
      ),
    )).filter((utxo: UTxO) => {
      const units = Object.keys(utxo.assets).filter((unit) =>
        unit !== "lovelace"
      );
      return units.every((unit) =>
        unit.startsWith(this.config.bond.escrowPolicyId)
      ) &&
        units.length >= 1;
    }).sort(sortAsc);
  }

  async getMyBondInWallet(): Promise<BondInfo[]> {
    return (await this.lucid.wallet.getUtxos()).filter(
      (utxo: UTxO) => {
        const units = Object.keys(utxo.assets).filter((unit) =>
          unit !== "lovelace"
        );
        return units.some((unit) =>
          unit.startsWith(this.config.bond.bondPolicyId)
        ) &&
          units.length >= 1;
      },
    ).map((utxo: UTxO) => {
      return Object.entries(utxo.assets)
        .filter((
          [unit, qty],
        ) =>
          unit !== "lovelace" &&
          unit.startsWith(this.config.bond.bondPolicyId) &&
          qty != undefined && qty > 0
        ).map(([unit, qty]) => {
          return {
            outRef: {
              txHash: utxo.txHash,
              outputIndex: utxo.outputIndex,
            },
            bondTokenId: unit.replace(this.config.bond.bondPolicyId, ""),
            quantity: qty,
          };
        });
    }).flat();
  }

  /**
   * Return the current bids for a specific token sorted in descending order by price.
   * Or return the collection bids on any token within the collection (use 'Open' as option).
   * Or return swap bids (use 'Swap' as option).
   */
  async getBids(
    option: "Bundle" | "Open" | "Swap" | { assetName: string },
  ): Promise<UTxO[]> {
    const bidAssetName = (() => {
      if (option === "Open") return fromText("BidOpen");
      if (option === "Swap") return fromText("BidSwap");
      if (option === "Bundle") return fromText("BidBundle");
      return fromText("Bid") + option.assetName;
    })();
    return (await this.lucid.utxosAtWithUnit(
      paymentCredentialOf(this.config.tradeAddress),
      toUnit(
        this.config.mintPolicyId,
        bidAssetName,
      ),
    )).filter((utxo: UTxO) => {
      const units = Object.keys(utxo.assets).filter((unit) =>
        unit !== "lovelace"
      );
      return units.every((unit) =>
        unit.startsWith(this.config.mintPolicyId) ||
        unit.startsWith(this.config.bond.bondPolicyId)
      ) &&
        (option === "Swap" ? units.length > 1 : units.length === 1);
    }).sort(sortDesc);
  }

  async utxoByUnit(policyId: PolicyId, assetName: AssetName): Promise<UTxO> {
    return await this.lucid.utxoByUnit(toUnit(policyId, assetName));
  }

  async utxosByUnit(policyId: PolicyId, assetName: AssetName): Promise<UTxO[]> {
    return await this.lucid.utxosByUnit(toUnit(policyId, assetName));
  }

  async utxosMintByUnit(
    policyId: PolicyId,
    assetName: AssetName,
  ): Promise<UTxO[]> {
    return await this.lucid.utxosMintByUnit(toUnit(policyId, assetName));
  }

  async getUtxosByHash(
    txHash: TxHash,
    ignoreLovelace: boolean,
  ): Promise<UTxO[]> {
    const utxos = await this.lucid.utxosByHash(txHash);

    return (ignoreLovelace
      ? utxos.filter((utxo: UTxO) => {
        return Object.keys(utxo.assets).filter((unit) => unit !== "lovelace");
      })
      : utxos).sort(sortDesc);
  }

  addressOf(cbor: string): AddressDetails {
    return this.lucid.utils.getAddressDetails(cbor);
  }

  async listingDatumOf(listingUtxo: UTxO): Promise<D.CadogoBondListingDatum> {
    return await this.lucid.datumOf<D.CadogoBondListingDatum>(
      listingUtxo,
      D.CadogoBondListingDatum,
    );
  }

  async getDeployedScripts(): Promise<{ trade: UTxO | null }> {
    if (!this.config.deployHash) return { trade: null };
    const [trade] = await this.lucid.utxosByOutRef([{
      txHash: this.config.deployHash,
      outputIndex: 0,
    }]);
    return { trade };
  }

  getContractHashes(): {
    scriptHash: ScriptHash;
    bondPolicyId: PolicyId;
    escrowPolicyId: PolicyId;
    bidPolicyId: PolicyId;
  } {
    return {
      scriptHash: this.config.tradeHash,
      bondPolicyId: this.config.bond.bondPolicyId,
      escrowPolicyId: this.config.bond.escrowPolicyId,
      bidPolicyId: this.config.mintPolicyId,
    };
  }

  /**
   * List asset(s) for a specified lovelace value. Optionally the listing could be private.\
   * Assets can be specified as either an array of asset names
   * (assuming each asset has a quantity of 1) or as a map,
   * where the quantity of each asset can be chosen.
   */
  async _list(
    assets: NameAndQuantity | AssetName[],
    requestedYield: number,
  ): Promise<Tx> {
    if (!(requestedYield > 1 && requestedYield < 9999)) {
      throw new Error("requested yield must be in range 1-9999");
    }
    const assetsMap: NameAndQuantity = assets instanceof Array
      ? Object.fromEntries(assets.map((assetName) => [assetName, 1n]))
      : assets;
    if (Object.keys(assetsMap).length <= 0) {
      throw new Error("Needs at least one asset.");
    }
    const ownerAddress = await this.lucid.wallet.address();
    const { stakeCredential } = this.lucid.utils
      .getAddressDetails(
        ownerAddress,
      );

    // We include the stake key of the signer
    const adjustedTradeAddress = stakeCredential
      ? this.lucid.utils.credentialToAddress(
        this.lucid.utils.scriptHashToCredential(this.config.tradeHash),
        stakeCredential,
      )
      : this.config.tradeAddress;

    const listingAssets: Assets = Object.fromEntries(
      Object.entries(assetsMap).map(
        (
          [assetName, quantity],
        ) => [toUnit(this.config.bond.bondPolicyId, assetName), quantity],
      ),
    );

    const ownerAddressInfo = this.getOwnerAddressInfo(
      fromAddress(ownerAddress),
    );
    if (ownerAddressInfo.paymentKey.type == "Script") {
      throw new Error(
        "Not support wallet address with payment key is script type",
      );
    }
    const inlineListingDatum = Data.to<D.CadogoBondListingDatum>(
      {
        ownerPaymentKey: ownerAddressInfo.paymentKey.hash,
        ownerStakeKey: ownerAddressInfo.stakeKey
          ? ownerAddressInfo.stakeKey.hash
          : null,
        requestedYield: BigInt(requestedYield),
      },
      D.CadogoBondListingDatum,
    );
    return this.lucid.newTx().payToContract(adjustedTradeAddress, {
      inline: inlineListingDatum,
    }, listingAssets);
  }

  getOwnerAddressInfo(
    address: D.Address,
  ): { paymentKey: Credential; stakeKey: Credential | undefined } {
    return {
      paymentKey: (() => {
        if ("PublicKeyCredential" in address.paymentCredential) {
          return this.lucid.utils.keyHashToCredential(
            address.paymentCredential.PublicKeyCredential[0],
          );
        } else {
          return this.lucid.utils.scriptHashToCredential(
            address.paymentCredential.ScriptCredential[0],
          );
        }
      })(),
      stakeKey: (() => {
        if (!address.stakeCredential) return undefined;
        if ("Inline" in address.stakeCredential) {
          if ("PublicKeyCredential" in address.stakeCredential.Inline[0]) {
            return this.lucid.utils.keyHashToCredential(
              address.stakeCredential.Inline[0].PublicKeyCredential[0],
            );
          } else {
            return this.lucid.utils.scriptHashToCredential(
              address.stakeCredential.Inline[0].ScriptCredential[0],
            );
          }
        } else {
          return undefined;
        }
      })(),
    };
  }

  /**
   * A bid can be placed on a specific token or a bundle within a collection
   * by specifying the assets as either an array of asset names
   * (assuming each asset has a quantity of 1) or as a map,
   * where the quantity of each asset can be chosen.
   */
  async _bid(
    assets: NameAndQuantity | AssetName[],
    lovelace: Lovelace,
  ): Promise<Tx> {
    const assetsMap: NameAndQuantity = assets instanceof Array
      ? Object.fromEntries(assets.map((assetName) => [assetName, 1n]))
      : assets;
    const bidNames = Object.keys(assetsMap);
    if (bidNames.length <= 0) {
      throw new Error("Needs at least one asset name.");
    }
    const ownerAddress = await this.lucid.wallet.address();
    const { stakeCredential } = this.lucid.utils.getAddressDetails(
      ownerAddress,
    );
    const bidAssets: Assets = Object.fromEntries(
      Object.entries(assetsMap).map(
        (
          [assetName, quantity],
        ) => [toUnit(this.config.bond.bondPolicyId, assetName), quantity],
      ),
    );

    const bidAssetName = bidNames.length > 1
      ? fromText("BidBundle")
      : fromText("Bid") + bidNames[0];

    // We include the stake key of the signer
    const adjustedTradeAddress = stakeCredential
      ? this.lucid.utils.credentialToAddress(
        this.lucid.utils.scriptHashToCredential(this.config.tradeHash),
        stakeCredential,
      )
      : this.config.tradeAddress;

    const biddingDatum: D.TradeDatum = {
      Bid: [{
        owner: fromAddress(ownerAddress),
        requestedOption: {
          SpecificValue: [
            fromAssets(bidAssets),
          ],
        },
      }],
    };

    return this.lucid.newTx()
      .mintAssets({
        [toUnit(this.config.mintPolicyId, bidAssetName)]: 1n,
      })
      .payToContract(adjustedTradeAddress, {
        inline: Data.to<D.TradeDatum>(biddingDatum, D.TradeDatum),
      }, {
        lovelace,
        [toUnit(this.config.mintPolicyId, bidAssetName)]: 1n,
      })
      .validFrom(this.lucid.utils.slotToUnixTime(1000))
      .attachMintingPolicy(this.config.mintPolicy);
  }

  /** Create a bid on any token within the collection. Optionally add constraints. */
  async _bidOpen(
    lovelace: Lovelace,
    constraints?: Constraints,
  ): Promise<Tx> {
    const ownerAddress = await this.lucid.wallet.address();
    const { stakeCredential } = this.lucid.utils.getAddressDetails(
      ownerAddress,
    );

    const adjustedTradeAddress = stakeCredential
      ? this.lucid.utils.credentialToAddress(
        this.lucid.utils.scriptHashToCredential(this.config.tradeHash),
        stakeCredential,
      )
      : this.config.tradeAddress;

    const biddingDatum: D.TradeDatum = {
      Bid: [{
        owner: fromAddress(ownerAddress),
        requestedOption: {
          SpecificSymbolWithConstraints: [
            this.config.bond.bondPolicyId,
            constraints?.types ? constraints.types.map(fromText) : [],
            constraints?.traits
              ? constraints.traits.map((
                { negation, trait },
              ) =>
                negation
                  ? { Excluded: [fromText(trait)] }
                  : { Included: [fromText(trait)] }
              )
              : [],
          ],
        },
      }],
    };

    return this.lucid.newTx()
      .mintAssets({
        [toUnit(this.config.mintPolicyId, fromText("BidOpen"))]: 1n,
      })
      .payToContract(adjustedTradeAddress, {
        inline: Data.to<D.TradeDatum>(biddingDatum, D.TradeDatum),
      }, {
        lovelace,
        [toUnit(this.config.mintPolicyId, fromText("BidOpen"))]: 1n,
      })
      .validFrom(this.lucid.utils.slotToUnixTime(1000))
      .attachMintingPolicy(this.config.mintPolicy);
  }

  /** Swap asset(s) for another asset(s). Ada could also be included on the offering side. */
  async _bidSwap(
    offering: {
      lovelace?: Lovelace;
      assetNames: string[];
    },
    requesting: {
      constraints?: Constraints;
      specific?: string[];
    },
  ): Promise<Tx> {
    if (
      [requesting.constraints, requesting.specific].filter((t) => t).length !==
        1
    ) {
      throw new Error(
        "You can/must have either constraints or a specific request.",
      );
    }
    if (offering.assetNames.length <= 0) {
      throw new Error("Needs at least one offering asset name.");
    }
    if (requesting.specific && requesting.specific.length <= 0) {
      throw new Error("Needs at least one requesting asset name.");
    }
    const ownerAddress = await this.lucid.wallet.address();
    const { stakeCredential } = this.lucid.utils.getAddressDetails(
      ownerAddress,
    );

    const adjustedTradeAddress = stakeCredential
      ? this.lucid.utils.credentialToAddress(
        this.lucid.utils.scriptHashToCredential(this.config.tradeHash),
        stakeCredential,
      )
      : this.config.tradeAddress;

    const biddingDatum: D.TradeDatum = {
      Bid: [{
        owner: fromAddress(ownerAddress),
        requestedOption: requesting.specific
          ? {
            SpecificValue: [
              fromAssets(
                Object.fromEntries(
                  requesting.specific.map(
                    (
                      assetName,
                    ) => [toUnit(this.config.bond.bondPolicyId, assetName), 1n],
                  ),
                ),
              ),
            ],
          }
          : {
            SpecificSymbolWithConstraints: [
              this.config.bond.bondPolicyId,
              requesting.constraints?.types
                ? requesting.constraints.types.map(fromText)
                : [],
              requesting.constraints?.traits
                ? requesting.constraints.traits.map((
                  { negation, trait },
                ) =>
                  negation
                    ? { Excluded: [fromText(trait)] }
                    : { Included: [fromText(trait)] }
                )
                : [],
            ],
          },
      }],
    };

    const offeringAssets: Assets = Object.fromEntries(
      offering.assetNames.map(
        (assetName) => [toUnit(this.config.bond.bondPolicyId, assetName), 1n],
      ),
    );
    if (offering.lovelace) offeringAssets.lovelace = offering.lovelace;

    return this.lucid.newTx()
      .mintAssets({
        [toUnit(this.config.mintPolicyId, fromText("BidSwap"))]: 1n,
      })
      .payToContract(adjustedTradeAddress, {
        inline: Data.to<D.TradeDatum>(biddingDatum, D.TradeDatum),
      }, {
        ...offeringAssets,
        [toUnit(this.config.mintPolicyId, fromText("BidSwap"))]: 1n,
      })
      .validFrom(this.lucid.utils.slotToUnixTime(1000))
      .attachMintingPolicy(this.config.mintPolicy);
  }

  getOwnerAddressOfListingDatum(
    listingDatum: D.CadogoBondListingDatum,
  ): Address {
    const listingDatumOwner: D.Address = {
      paymentCredential: {
        PublicKeyCredential: [listingDatum.ownerPaymentKey],
      },
      stakeCredential: listingDatum.ownerStakeKey
        ? {
          Inline: [
            {
              PublicKeyCredential: [listingDatum.ownerStakeKey],
            },
          ],
        }
        : null,
    };
    return toAddress(listingDatumOwner, this.lucid);
  }

  async _cancelListing(listingUtxo: UTxO): Promise<Tx> {
    const listingDatum = await this.lucid.datumOf<D.CadogoBondListingDatum>(
      listingUtxo,
      D.CadogoBondListingDatum,
    );
    if (
      !("ownerPaymentKey" in listingDatum && "requestedYield" in listingDatum)
    ) {
      throw new Error("Not a listing UTxO");
    }
    const owner: Address = this.getOwnerAddressOfListingDatum(listingDatum);
    const ownerKey = paymentCredentialOf(owner).hash;

    const address: Address = await this.lucid.wallet.address();

    if (ownerKey !== paymentCredentialOf(address).hash) {
      throw new Error("You are not the owner.");
    }

    const refScripts = await this.getDeployedScripts();
    return this.lucid.newTx().collectFrom(
      [listingUtxo],
      Data.to<D.CadogoBondTradeAction>("Update", D.CadogoBondTradeAction),
    )
      .addSignerKey(ownerKey)
      .compose(
        refScripts.trade
          ? this.lucid.newTx().readFrom([refScripts.trade])
          : this.lucid.newTx().attachSpendingValidator(
            this.config.tradeValidator,
          ),
      );
  }

  async _sell(
    bidUtxo: UTxO,
    assetName?: string,
  ): Promise<Tx> {
    const tradeDatum = await this.lucid.datumOf<D.TradeDatum>(
      bidUtxo,
      D.TradeDatum,
    );
    if (!("Bid" in tradeDatum)) {
      throw new Error("Not a bidding UTxO");
    }

    const bidDetails = tradeDatum.Bid[0];

    const { lovelace } = bidUtxo.assets;
    const bidToken = Object.keys(bidUtxo.assets).find((unit) =>
      unit.startsWith(this.config.mintPolicyId)
    );
    if (!bidToken) throw new Error("No bid token found.");

    const owner: Address = toAddress(bidDetails.owner, this.lucid);

    const { requestedAssets, refNFT } = (() => {
      if ("SpecificValue" in bidDetails.requestedOption) {
        return {
          requestedAssets: toAssets(
            bidDetails.requestedOption.SpecificValue[0],
          ),
          refNFT: null,
        };
      } else if (
        "SpecificSymbolWithConstraints" in bidDetails.requestedOption &&
        assetName
      ) {
        const policyId: PolicyId =
          bidDetails.requestedOption.SpecificSymbolWithConstraints[0];
        const refNFT = toUnit(
          policyId,
          fromUnit(toUnit(policyId, assetName)).name,
          100,
        );
        const types =
          bidDetails.requestedOption.SpecificSymbolWithConstraints[1];
        const traits =
          bidDetails.requestedOption.SpecificSymbolWithConstraints[2];

        return {
          requestedAssets: {
            [toUnit(policyId, assetName)]: 1n,
          },
          refNFT: types.length > 0 || traits.length > 0 ? refNFT : null,
        };
      }
      throw new Error("No variant matched.");
    })();

    const paymentDatum = Data.to<D.PaymentDatum>({
      outRef: {
        txHash: { hash: bidUtxo.txHash },
        outputIndex: BigInt(bidUtxo.outputIndex),
      },
    }, D.PaymentDatum);

    const refScripts = await this.getDeployedScripts();

    return this.lucid.newTx()
      .collectFrom(
        [bidUtxo],
        Data.to<D.CadogoBondTradeAction>("Sell", D.CadogoBondTradeAction),
      )
      .compose(
        refNFT
          ? await (async () => {
            const refUtxo = await this.lucid.utxoByUnit(refNFT!);
            if (!refUtxo) throw new Error("This NFT doesn't support CIP-0068");
            return this.lucid.newTx().readFrom([refUtxo]);
          })()
          : null,
      )
      .compose(
        (this._payFee(
          lovelace,
          paymentDatum,
        )).tx,
      )
      .payToAddressWithData(owner, {
        inline: paymentDatum,
      }, requestedAssets)
      .mintAssets({ [bidToken]: -1n })
      .validFrom(this.lucid.utils.slotToUnixTime(1000))
      .compose(
        refScripts.trade
          ? this.lucid.newTx().readFrom([refScripts.trade])
          : this.lucid.newTx().attachSpendingValidator(
            this.config.tradeValidator,
          ),
      )
      .attachMintingPolicy(this.config.mintPolicy);
  }

  async _cancelBid(bidUtxo: UTxO): Promise<Tx> {
    const tradeDatum = await this.lucid.datumOf<D.TradeDatum>(
      bidUtxo,
      D.TradeDatum,
    );
    if (!("Bid" in tradeDatum)) {
      throw new Error("Not a bidding UTxO");
    }
    const owner: Address = toAddress(tradeDatum.Bid[0].owner, this.lucid);
    const ownerKey = paymentCredentialOf(owner).hash;

    const address: Address = await this.lucid.wallet.address();

    if (ownerKey !== paymentCredentialOf(address).hash) {
      throw new Error("You are not the owner.");
    }

    const [bidToken] = Object.keys(bidUtxo.assets).filter((unit) =>
      unit.startsWith(this.config.mintPolicyId)
    );

    const refScripts = await this.getDeployedScripts();

    return this.lucid.newTx()
      .collectFrom(
        [bidUtxo],
        Data.to<D.CadogoBondTradeAction>("Update", D.CadogoBondTradeAction),
      )
      .mintAssets({ [bidToken]: -1n })
      .validFrom(this.lucid.utils.slotToUnixTime(1000))
      .addSignerKey(ownerKey)
      .compose(
        refScripts.trade
          ? this.lucid.newTx().readFrom([refScripts.trade])
          : this.lucid.newTx().attachSpendingValidator(
            this.config.tradeValidator,
          ),
      )
      .attachMintingPolicy(this.config.mintPolicy);
  }

  _getMillisecondsOfDay(): bigint {
    return this.config.bond.epochConfig.epochLength * 86400000n /
      this.config.bond.epochConfig.epochLengthBase;
  }

  _relativeEpochToPosixTimeStart(relativeEpoch: bigint): bigint {
    return (relativeEpoch - this.config.bond.epochConfig.epochBoundaryAsEpoch) *
        this.config.bond.epochConfig.epochLength +
      this.config.bond.epochConfig.epochBoundary;
  }

  _relativeEpochToPosixTimeEnd(relativeEpoch: bigint): bigint {
    return this._relativeEpochToPosixTimeStart(relativeEpoch) +
      this.config.bond.epochConfig.epochLength;
  }

  _posixTimeToRelativeEpoch(posix_time: bigint): bigint {
    return (posix_time - this.config.bond.epochConfig.epochBoundary) /
        this.config.bond.epochConfig.epochLength +
      this.config.bond.epochConfig.epochBoundaryAsEpoch;
  }

  _getCurrentEpoch(): bigint {
    return this._posixTimeToRelativeEpoch(
      BigInt((new Date()).getUTCMilliseconds()),
    );
  }

  _getDayToMaturity(txTime: bigint, maturityTime: bigint): bigint {
    return (maturityTime - txTime) / this._getMillisecondsOfDay();
  }

  _getPriceOfBond(
    receivedAtMaturity: bigint,
    dayToMaturity: bigint,
    requestedYield: bigint,
  ): bigint {
    return receivedAtMaturity * this.config.bond.basisPointsRefUnit *
      this.config.bond.basisPointsRefUnit / (
        this.config.bond.basisPointsRefUnit *
          this.config.bond.basisPointsRefUnit +
        requestedYield * this.config.bond.basisPointsRefUnit * dayToMaturity /
          365n
      );
  }

  _getEscrowInfo(
    escrowLovelace: bigint,
    escrowDatum: D.POpenDatum,
  ): { receivedAtMaturityOneBond: bigint; dayToMaturity: bigint } {
    const txTime = BigInt((new Date()).getTime());
    const currentEpoch = this._posixTimeToRelativeEpoch(txTime);
    const epochStart = escrowDatum.start +
      this.config.bond.epochConfig.epochBoundaryAsEpoch;
    const epochMaturity = epochStart + escrowDatum.duration;
    const epochRewardsLovelace = escrowDatum.epoRewards.get("")?.get("");
    if (epochRewardsLovelace == undefined) {
      throw new Error("Not found Epoch Rewards in Escrow Datum.");
    }
    const principalLovelace = escrowDatum.bondAmount *
      this.config.bond.bondFaceValue;
    const premiumPaidLovelace = escrowLovelace - principalLovelace;
    const totalEpochDuePaid = currentEpoch - epochStart + 1n;
    const totalInterestDuePaid = totalEpochDuePaid *
      this.config.bond.bondFaceValue;
    const interestLevelEpoch = (premiumPaidLovelace - totalInterestDuePaid) /
      epochRewardsLovelace;
    if (
      interestLevelEpoch < escrowDatum.buffer && currentEpoch >= epochMaturity
    ) throw new Error("This Bond is CLOSABLE");
    const lendAfterFee = this.config.bond.basisPointsRefUnit -
      escrowDatum.otmFee;
    const lenderInterest = BigInt(epochRewardsLovelace) * escrowDatum.duration *
      lendAfterFee;

    return {
      receivedAtMaturityOneBond:
        (lenderInterest / this.config.bond.basisPointsRefUnit /
          escrowDatum.bondAmount) + this.config.bond.bondFaceValue,
      dayToMaturity: this._getDayToMaturity(
        txTime,
        this._relativeEpochToPosixTimeStart(epochMaturity),
      ),
    };
  }

  async _buy(listingUtxo: UTxO, quantity: bigint): Promise<Tx> {
    const listingToken = Object.keys(listingUtxo.assets).find((unit) =>
      unit.startsWith(this.config.bond.bondPolicyId)
    );
    if (!listingToken) throw new Error("No listing token found.");

    const listingTokenQty = listingUtxo.assets[listingToken];
    if (quantity <= 0 || quantity > listingTokenQty) {
      throw new Error(
        "Buy qty must be greater than zero and less than or equal to quantity of listing Utxo.",
      );
    }

    const listingTokenUnit = fromUnit(listingToken);
    const escrowUtxo: UTxO = await this.lucid.utxoByUnit(
      toUnit(this.config.bond.escrowPolicyId, listingTokenUnit.assetName),
    );
    if (escrowUtxo == undefined) throw new Error("Not found Escrow Utxo.");

    const escrowUtxoDatum = await this.lucid.datumOf<D.POpenDatum>(
      escrowUtxo,
      D.POpenDatum,
    );
    if (escrowUtxoDatum == undefined) {
      throw new Error("Can't get Escrow Datum of listing UTxO");
    }
    const escrowInfo = this._getEscrowInfo(
      escrowUtxo.assets["lovelace"],
      escrowUtxoDatum,
    );

    const listingDatum = await this.lucid.datumOf<D.CadogoBondListingDatum>(
      listingUtxo,
      D.CadogoBondListingDatum,
    );
    if (
      !("ownerPaymentKey" in listingDatum && "requestedYield" in listingDatum)
    ) {
      throw new Error("Not a listing UTxO");
    }
    const receivedAtMaturity = escrowInfo.receivedAtMaturityOneBond * quantity;
    const priceOfOneBond = this._getPriceOfBond(
      escrowInfo.receivedAtMaturityOneBond,
      escrowInfo.dayToMaturity,
      listingDatum.requestedYield,
    );
    const receivedWithYield = priceOfOneBond * quantity;
    const receivedDiff = receivedAtMaturity - receivedWithYield;
    const marketFeeSeller = receivedDiff * this.config.market.feeSeller /
      this.config.bond.basisPointsRefUnit;
    const marketFeeBuyer = receivedDiff * this.config.market.feeBuyer /
      this.config.bond.basisPointsRefUnit;

    const owner: Address = this.getOwnerAddressOfListingDatum(listingDatum);

    // const paymentDatum = Data.to<D.PaymentDatum>({
    //   outRef: {
    //     txHash: { hash: listingUtxo.txHash },
    //     outputIndex: BigInt(listingUtxo.outputIndex),
    //   },
    // }, D.PaymentDatum);

    const listingLovelace = listingUtxo.assets["lovelace"];
    const marketAddressReceived = marketFeeSeller + marketFeeBuyer +
      (quantity < listingTokenQty ? 0n : listingLovelace);
    const ownerAddressReceived = receivedWithYield - marketFeeSeller;

    const refScripts = await this.getDeployedScripts();

    return this.lucid.newTx().collectFrom(
      [listingUtxo],
      Data.to<D.CadogoBondTradeAction>("Buy", D.CadogoBondTradeAction),
    )
      .payToAddress(this.config.market.address, {
        lovelace: marketAddressReceived,
      })
      .payToAddress(owner, { lovelace: ownerAddressReceived })
      .compose(
        (() => {
          if (quantity < listingTokenQty) {
            return this.lucid.newTx().payToContract(
              listingUtxo.address,
              {
                inline: Data.to<D.CadogoBondListingDatum>(
                  listingDatum,
                  D.CadogoBondListingDatum,
                ),
              },
              Object.fromEntries([["lovelace", listingLovelace], [
                listingToken,
                listingTokenQty - quantity,
              ]]),
            );
          } else return this.lucid.newTx();
        })(),
      )
      .compose(
        refScripts.trade
          ? this.lucid.newTx().readFrom([refScripts.trade])
          : this.lucid.newTx().attachSpendingValidator(
            this.config.tradeValidator,
          ),
      ).readFrom([escrowUtxo]);
  }

  private _payFee(
    lovelace: Lovelace,
    paymentDatum: Datum,
  ): { tx: Tx; remainingLovelace: Lovelace } {
    const tx = this.lucid.newTx();

    let remainingLovelace = lovelace;

    const address: Address = this.config.market.address;
    const fee = this.config.market.feeBuyer;
    const minFee = this.config.market.minFee;
    const maxFee = this.config.market.maxFee;

    const feeToPay = (lovelace * 10n) / fee;
    const adjustedFee = minFee && feeToPay < minFee
      ? minFee
      : maxFee && feeToPay > maxFee
      ? maxFee
      : feeToPay;

    remainingLovelace -= adjustedFee;

    tx.payToAddressWithData(address, { inline: paymentDatum }, {
      lovelace: adjustedFee,
    });

    // max(0, remainingLovelace)
    remainingLovelace = remainingLovelace < 0n ? 0n : remainingLovelace;

    return { tx, remainingLovelace };
  }
}
