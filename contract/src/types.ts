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

export type EpochConfig = {
  // 1 year = 365 days / 5 days per epoch = 73 epoch. default: 73
  yearToEpoch: bigint,
  // default: 1_647_899_091_000
  epochBoundary: bigint,
  // default: epoch 327+1
  epochBoundaryAsEpoch: bigint,
  // Each Cardano epoch consists of a number of slots, where each slot lasts for one second
  // A Cardano epoch currently includes 432,000 slots (5 days)
  // mainnet: 432_000_000
  // preview: 1_800_000
  epochLength: bigint,
  // base is 5 days = 432_000_000
  epochLengthBase: bigint,
}

export type BondConfig = {
  bondPolicyId: PolicyId,
  escrowPolicyId: PolicyId,
  // basis_points reference value, default: 10_000
  basisPointsRefUnit: bigint,
  // range basis_points min = 1, max = 9_999
  basisPointsMin: bigint,
  basisPointsMax: bigint,
  oneAda2Lovelace: bigint,
  // 1 bond = 100 ADA = 100_000_000 lovelace. default: 100 ADA = 100_000_000 lovelace
  bondFaceValue: bigint,
  epochConfig: EpochConfig,
}

export type SlotConfigNetwork = {
  zero_time: bigint,
  zero_slot: bigint,
  slot_length: bigint,
}

export type CadogoConfig = {
  bond: BondConfig,
  market: MarketFee,
  slotConfigNetwork: SlotConfigNetwork,
  
  owner: Address;
  deployHash: TxHash;

  tradeValidator: SpendingValidator;
  tradeHash: ScriptHash;
  tradeAddress: Address;
  mintPolicy: MintingPolicy;
  mintPolicyId: PolicyId;
}

export type BondInfo = {
  outRef: OutRef;
  bondTokenId: AssetName;
  quantity: bigint;
};
