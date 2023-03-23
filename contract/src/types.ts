import {
  Address,
  Lovelace,
  MintingPolicy,
  OutRef,
  PolicyId,
  ScriptHash,
  SpendingValidator,
  TxHash,
  Unit,
} from "../../deps.ts";

export type ContractConfig = {
  royaltyToken: Unit;
  policyId: PolicyId;
  fundProtocol?: boolean;
  owner?: Address;
  deployHash?: TxHash;
  aggregatorFee?: RoyaltyRecipient[];
};

export type RoyaltyRecipient = {
  address: Address;
  /** Variable fee. e.g.: 0.04 (4%) */
  fee: number;
  /** Optionally set a minimum absolute fee. */
  minFee?: Lovelace | null;
  /** Optionally set a maximum absolute fee. */
  maxFee?: Lovelace | null;
};

export type Constraints = {
  types?: string[];
  traits?: { negation?: boolean; trait: string }[];
};

export type AssetName = string;
export type NameAndQuantity = Record<AssetName, bigint>;

export type MarketConfig = {
  policyIdOfBond: PolicyId;
  policyIdOfEscrow: PolicyId;
  owner: Address;
  deployHash: TxHash;
  marketFee: MarketFee;

  tradeValidator: SpendingValidator;
  tradeHash: ScriptHash;
  tradeAddress: Address;
  mintPolicy: MintingPolicy;
  mintPolicyId: PolicyId;
};

export type MarketFee = {
  address: Address;
  /** Variable fee. e.g.: 0.04 (4%) */
  feeBuyer: bigint;
  feeSeller: bigint;
  /** Optionally set a minimum absolute fee. */
  minFee?: Lovelace | null;
  /** Optionally set a maximum absolute fee. */
  maxFee?: Lovelace | null;
};

export type BondInfo = {
  outRef: OutRef;
  bondTokenId: AssetName;
  quantity: bigint;
};
