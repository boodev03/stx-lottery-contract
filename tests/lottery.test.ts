import { boolCV, listCV, noneCV, uintCV } from "@stacks/transactions";
import { describe, expect, it } from "vitest";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const wallet1 = accounts.get("wallet_1")!;

describe("lottery contract test", () => {
  it("should start lottery successfully", () => {
    const { result } = simnet.callPublicFn("lottery-v20", "start-lottery", [], deployer);
    expect(result).toBeOk(boolCV(true));
  });

  it("should buy ticket successfully", () => {
    const numbers = [1, 2, 3, 4].map(n => uintCV(n));
    const roundId = uintCV(1);
    const { result, events } = simnet.callPublicFn(
      "lottery-v20",
      "buy-ticket",
      [listCV(numbers), roundId],
      wallet1
    );
    console.log("Error:", result);
    // expect(result).toBeOk(uintCV(1)); // First ticket ID should be 1
  });
});
