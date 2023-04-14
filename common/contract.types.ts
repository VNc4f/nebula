import {Data} from "../deps.ts";

export const Credential = Data.Enum([
  Data.Object({PublicKeyCredential: Data.Tuple([Data.String])}),
  Data.Object({ScriptCredential: Data.Tuple([Data.String])}),
]);
export type Credential = Data.Static<typeof Credential>;

export const StakeCredential = Data.Enum([
  Data.Object({Inline: Data.Tuple([Credential])}),
  Data.Object({
    Pointer: Data.Tuple([Data.Object({
      slotNumber: Data.BigInt,
      transactionIndex: Data.BigInt,
      certificateIndex: Data.BigInt,
    })]),
  }),
]);
export type StakeCredential = Data.Static<typeof StakeCredential>;

export const Address = Data.Object({
  paymentCredential: Credential,
  stakeCredential: Data.Nullable(StakeCredential),
});
export type Address = Data.Static<typeof Address>;

export const Value = Data.Map(Data.String, Data.Map(Data.String, Data.BigInt));
export type Value = Data.Static<typeof Value>;

export const TraitOption = Data.Enum([
  Data.Object({Included: Data.Tuple([Data.String])}),
  Data.Object({Excluded: Data.Tuple([Data.String])}),
]);
export type TraitOption = Data.Static<typeof TraitOption>;

export const BidOption = Data.Enum([
  Data.Object({SpecificValue: Data.Tuple([Value])}),
  Data.Object({
    SpecificSymbolWithConstraints: Data.Tuple([
      Data.String,
      Data.Array(Data.String),
      Data.Array(TraitOption),
    ]),
  }),
]);
export type BidOption = Data.Static<typeof BidOption>;

export const OutRef = Data.Object({
  txHash: Data.Object({hash: Data.String}),
  outputIndex: Data.BigInt,
});
export type OutRef = Data.Static<typeof OutRef>;

export const ListingDetails = Data.Object({
  owner: Address,
  requestedLovelace: Data.BigInt,
  privateListing: Data.Nullable(Address),
});
export type ListingDetails = Data.Static<typeof ListingDetails>;

export const BiddingDetails = Data.Object({
  owner: Address,
  requestedOption: BidOption,
});
export type BiddingDetails = Data.Static<typeof BiddingDetails>;

export const RoyaltyRecipient = Data.Object({
  address: Address,
  fee: Data.BigInt,
  minFee: Data.Nullable(Data.BigInt),
  maxFee: Data.Nullable(Data.BigInt),
});
export type RoyaltyRecipient = Data.Static<typeof RoyaltyRecipient>;

export const RoyaltyInfo = Data.Object({
  recipients: Data.Array(RoyaltyRecipient),
  version: Data.BigInt,
  extra: Data.Any,
});
export type RoyaltyInfo = Data.Static<typeof RoyaltyInfo>;

export const RoyaltyToken = Data.Object({
  policyId: Data.String,
  assetName: Data.String,
});
export type RoyaltyToken = Data.Static<typeof RoyaltyToken>;

export const PaymentDatum = Data.Object({
  outRef: OutRef,
});
export type PaymentDatum = Data.Static<typeof PaymentDatum>;

export const TradeAction = Data.Enum([
  Data.Literal("Sell"),
  Data.Literal("Buy"),
  Data.Literal("Cancel"),
]);
export type TradeAction = Data.Static<typeof TradeAction>;

export const TradeDatum = Data.Enum([
  Data.Object({Listing: Data.Tuple([ListingDetails])}),
  Data.Object({Bid: Data.Tuple([BiddingDetails])}),
]);
export type TradeDatum = Data.Static<typeof TradeDatum>;

export const TradeParams = Data.Tuple([
  Data.Nullable(Data.String),
  RoyaltyToken,
]);
export type TradeParams = Data.Static<typeof TradeParams>;

export const Balance = Data.Object({
  lovelace: Data.BigInt,
  assets: Value,
}, false);
export type Balance = Data.Static<typeof Balance>;


export const ListingSample = Data.Object({
  owner: Data.String,
  amount: Data.BigInt,
  private: Data.Boolean,
  tuple: Data.Tuple([Value]),
  tuple_constr: Data.Tuple([Value], true),
  enum: Data.Enum([
    Data.Literal("Sell"),
    Data.Literal("Buy"),
    Data.Literal("Cancel"),
  ]),
  nullable: Data.Nullable(Data.BigInt),
  obj: Data.Object({
    policyId: Data.String,
    assetName: Data.String,
  }),
  arr: Data.Nullable(Data.Array(Data.String)),
  map: Value,
});
export type ListingSample = Data.Static<typeof ListingSample>;

// Optim Bond
/*
data PBondWriterDatum (s :: S)
  = PBondWriterDatum (Term s (PDataRecord
      '[ "epoRewards"         ':= PValue 'Sorted 'NoGuarantees      -- ^ monthly value gain promised
       , "duration"           ':= PEpochTime                        -- ^ duration of the loan in epochs
       , "bondSymbol"         ':= PCurrencySymbol                   -- ^ bond token currency symbol
       , "tokenName"          ':= PTokenName                        -- ^ token name of both Uniqueness and Ownership NFTs
       , "bondAmount"         ':= PInteger                          -- ^ amount of bond
       , "buffer"             ':= PEpochTime                        -- ^ how many epochs in advance the position needs to be maintained
       , "otmFee"             ':= PBasisPoints                      -- ^ Optim fee
       , "stakeKey"           ':= PStakingCredential                -- ^ stake key for open state
       , "permissioned"       ':= PMaybeData (PAsData PPubKeyHash)  -- ^ optional key required to purchase this bond
       ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PTryFrom PData)
instance DerivePlutusType PBondWriterDatum where type DPTStrat _ = PlutusTypeData
 */
export const PBondWriterDatum = Data.Object({
  epoRewards: Value,
  duration: Data.BigInt,
  bondSymbol: Data.String,
  tokenName: Data.String,
  bondAmount: Data.BigInt,
  buffer: Data.BigInt,
  otmFee: Data.BigInt,
  stakeKey: StakeCredential,
  permissioned: Data.Nullable(Data.Tuple([Data.String])),
});
export type PBondWriterDatum = Data.Static<typeof PBondWriterDatum>;

/*
newtype POpenPoolDatum (s :: S)
  = POpenPoolDatum (Term s (PDataRecord
      '[ "minEpoRewards" ':= PValue 'Sorted 'NoGuarantees
       , "minPrepaid"    ':= PEpochTime
       , "minBuffer"     ':= PEpochTime
       , "bondSymbol"    ':= PCurrencySymbol
       , "specificBond"  ':= PMaybeData PAssetClass
       , "poolToken"     ':= PTokenName
       ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PTryFrom PData)
instance DerivePlutusType POpenPoolDatum where type DPTStrat _ = PlutusTypeData
*/
// asset_name: "82f2356d37f02d3f4c1d3ad2af585ea1b3e485830a2fd3af0f4b07113cf23496"
// datum_hash: "90d666aeba26dc9bdf1a7bda57e5a8f5a141bc283269b8ba4c7090af5c66fb42"
// policy_id: "4702f1ff21a54f728a59b3f5f0f351891c99015a2158b816c721ea72"
export const POpenPoolDatum = Data.Object({
  minEpoRewards: Value,
  minPrepaid: Data.BigInt,
  minBuffer: Data.BigInt,
  bondSymbol: Data.String,
  specificBond: Data.Nullable(Data.Object({
    currencySymbol: Data.String,
    tokenName: Data.String,
  })),
  poolToken: Data.String,
});
export type POpenPoolDatum = Data.Static<typeof POpenPoolDatum>;

/*
data PClosedPoolDatum (s :: S)
    = PClosedPoolDatum (Term s (PDataRecord
    '[ "bondSymbol"         ':= PCurrencySymbol
     , "bondToken"          ':= PTokenName
     , "poolToken"          ':= PTokenName
     ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PTryFrom PData)
instance DerivePlutusType PClosedPoolDatum where type DPTStrat _ = PlutusTypeData
 */
// asset_name: "3b7788e0ddcd9095d782dbbc4230c5cbaf406a5dfb542cf17445f81b010a9cca"
// datum_hash: "70168af76576b245d4a66063d084df6d643a86a5677f287222674552c4067639"
export const PClosedPoolDatum = Data.Object({
  bondSymbol: Data.String,
  bondToken: Data.String,
  poolToken: Data.String,
});
export type PClosedPoolDatum = Data.Static<typeof PClosedPoolDatum>;

/*
data POpenDatum (s :: S)
  = POpenDatum (Term s (PDataRecord
'[ "epoRewards"      ':= PValue 'Sorted 'NoGuarantees  -- ^ monthly lovelace value gain promised
  , "duration"        ':= PEpochTime                    -- ^ duration of the loan in epochs
  , "bondSymbol"      ':= PCurrencySymbol
  , "tokenName"       ':= PTokenName                    -- ^ token name of both Uniqueness and Ownership NFTs
  , "bondAmount"      ':= PInteger
  , "buffer"          ':= PEpochTime                    -- ^ number of epochs paid in advance
  , "otmFee"          ':= PBasisPoints                  -- ^ Optim fee
  , "ogLender"        ':= PPubKeyHash                   -- ^ used in staking the funds back to them
  , "start"           ':= PEpochTime
]))
deriving stock (Generic)
deriving anyclass (PlutusType, PIsData, PDataFields, PTryFrom PData)
instance DerivePlutusType POpenDatum where type DPTStrat _ = PlutusTypeData
*/
// asset_name: "82f2356d37f02d3f4c1d3ad2af585ea1b3e485830a2fd3af0f4b07113cf23496"
// datum_hash: "be29a8209c94cffae2d94e51721439074472dc2a3dfef4da8342c989ca8a39ad"
// policy_id: "5f1dd3192cbdaa2c1a91560a6147466efb18d33a5d6516b266ce6b6f"
export const POpenDatum = Data.Object({
  epoRewards: Value,
  duration: Data.BigInt,
  bondSymbol: Data.String,
  tokenName: Data.String,
  bondAmount: Data.BigInt,
  buffer: Data.BigInt,
  otmFee: Data.BigInt,
  ogLender: Data.String,
  start: Data.BigInt,
});
export type POpenDatum = Data.Static<typeof POpenDatum>;

export const PBondUnknownDatum = Data.Object({
  unknown: Data.Array(Data.Array(Data.Any)),
});
export type PBondUnknownDatum = Data.Static<typeof PBondUnknownDatum>;






export const SlotConfigNetwork = Data.Object({
  zeroTime: Data.BigInt,
  zeroSlot: Data.BigInt,
  slotLength: Data.BigInt,
});
export type SlotConfigNetwork = Data.Static<typeof SlotConfigNetwork>;

export const BondMarketConfig = Data.Object({
  bondPolicyId:Data.String,
  escrowPolicyId:Data.String,
  // 1 bond = 100 ADA = 100_000_000 lovelace. default: 100 ADA = 100_000_000 lovelace
  bondFaceValue: Data.BigInt,
  // 1 year = 365 days/ 5 days per epoch = 73 epoch. default: 73
  yearToEpoch: Data.BigInt,
  // default: 1647899091000
  epochBoundary: Data.BigInt,
  // Each Cardano epoch consists of a number of slots, where each slot lasts for one second
  // A Cardano epoch currently includes 432,000 slots (5 days)
  // default: 432_000_000
  epochLength: Data.BigInt,
  minPrepaid: Data.BigInt,
  // default: epoch 327
  epochBoundaryAsEpoch: Data.BigInt,
  marketFeeAddress: Address,
  // value in range 0001 -> 9999 default: 1% per yield == 100
  marketFeeBuyer: Data.BigInt,
  // value in range 0001 -> 9999 default: 1% per yield == 100
  marketFeeSeller: Data.BigInt,
  SlotConfigNetwork: SlotConfigNetwork,
});
export type BondMarketConfig = Data.Static<typeof BondMarketConfig>;

export const ListingAction = Data.Enum([
  Data.Literal("Sell"),
  Data.Literal("Update"),
  Data.Literal("Buy"),
  // Data.Literal("WithdrawForTest"),
]);
export type ListingAction = Data.Static<typeof ListingAction>;

export const ListingDatum = Data.Object({
  owner: Address,
  requestedYield: Data.BigInt,
});
export type ListingDatum = Data.Static<typeof ListingDatum>;



export const CadogoBondTradeAction = Data.Enum([
  Data.Literal("Update"),
  Data.Literal("Buy"),
  Data.Literal("Sell"),
  // Data.Literal("Withdraw"),
]);
export type CadogoBondTradeAction = Data.Static<typeof CadogoBondTradeAction>;

export const CadogoBondListingDatum = Data.Object({
  ownerPaymentKey: Data.String,
  ownerStakeKey: Data.Nullable(Data.String),
  requestedYield: Data.BigInt,
});
export type CadogoBondListingDatum = Data.Static<typeof CadogoBondListingDatum>;

export const CadogoEpochConfig = Data.Object({
  // 1 year = 365 days / 5 days per epoch = 73 epoch. default: 73
  yearToEpoch: Data.BigInt,
  // default: 1_647_899_091_000
  epochBoundary: Data.BigInt,
  // default: epoch 327+1
  epochBoundaryAsEpoch: Data.BigInt,
  // Each Cardano epoch consists of a number of slots, where each slot lasts for one second
  // A Cardano epoch currently includes 432,000 slots (5 days)
  // mainnet: 432_000_000
  // preview: 1_800_000
  epochLength: Data.BigInt,
  // base is 5 days = 432_000_000
  epochLengthBase: Data.BigInt,
});
export type CadogoEpochConfig = Data.Static<typeof CadogoEpochConfig>;

export const CadogoBondConfig = Data.Object({
  bondPolicyId: Data.String,
  escrowPolicyId: Data.String,
  // basis_points reference value, default: 10_000
  basisPointsRefUnit: Data.BigInt,
  // range basis_points min = 1, max = 9_999
  basisPointsMin: Data.BigInt,
  basisPointsMax: Data.BigInt,
  // default min prepaid xem not found duration config in dict min_prepaid
  oneAda2Lovelace: Data.BigInt,
  // 1 bond = 100 ADA = 100_000_000 lovelace. default: 100 ADA = 100_000_000 lovelace
  bondFaceValue: Data.BigInt,
  epochConfig: CadogoEpochConfig,
});
export type CadogoBondConfig = Data.Static<typeof CadogoBondConfig>;

export const CadogoBondMarketConfig = Data.Object({
  address: Address,
  // value in range 0001 -> 9999 default: 1% per yield == 100
  buyerFee: Data.BigInt,
  // value in range 0001 -> 9999 default: 1% per yield == 100
  sellerFee: Data.BigInt,
});
export type CadogoBondMarketConfig = Data.Static<typeof CadogoBondMarketConfig>;

export const CadogoConfig = Data.Object({
  bond: CadogoBondConfig,
  market: CadogoBondMarketConfig,
  slotConfigNetwork: SlotConfigNetwork,
});
export type CadogoConfig = Data.Static<typeof CadogoConfig>;

export const CadogoMarketParams = Data.Tuple([
  CadogoConfig,
]);
export type CadogoMarketParams = Data.Static<typeof CadogoMarketParams>;





export const BiddingDatum = Data.Object({
  owner: Address,
  fromEpoch: Data.BigInt,
  toEpoch: Data.BigInt,
  quantity: Data.BigInt,
  requestedYield: Data.BigInt,
});
export type BiddingDatum = Data.Static<typeof BiddingDatum>;