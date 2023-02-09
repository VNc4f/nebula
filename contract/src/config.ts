import { ContractConfig } from "./types.ts";

export const budConfig: ContractConfig = {
  royaltyToken:
    "b914760147d2f4368ca79dbe0df968cac15944b5b931726e86568030001f4d70526f79616c7479",
  policyId: "f416604ad7f0f646cbe87cdd16fa67542e1be51f70e258ee63111452",
  aggregatorFee: [{
    address: "addr_test1qp86gj4n7j7u38j72yxkage4vl878znhjjk5g0xjfnjjjc6m0ptkv7scap7cjfax7qzqsey4uz46g9trnhq5xn94jq8q47yely",
    fee: 0.04,
    minFee: 2000000n,
    maxFee: 3000000n,
  }],
  deployHash: "b7f49947a5071ce322bc3127ca79d942b13af7a88253c614be9935b6190376f",
};
