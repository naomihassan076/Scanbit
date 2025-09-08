import {
  Clarinet,
  Tx,
  Chain,
  Account,
  types
} from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
  name: "Test boost system functionality",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const user1 = accounts.get('wallet_1')!;

    let block = chain.mineBlock([
      // Create a QR code
      Tx.contractCall('scanbit', 'create-qr-code', [
        types.uint(100), // reward amount
        types.uint(10),  // max scans
        types.uint(1000), // duration blocks
        types.ascii("Test Location"), // location
        types.ascii("Test Category")  // category
      ], deployer.address)
    ]);

    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.uint(1));

    // Test activating a boost
    block = chain.mineBlock([
      Tx.contractCall('scanbit', 'activate-boost', [
        types.uint(1), // qr-id
        types.uint(2), // multiplier (2x)
        types.uint(100) // duration in blocks
      ], deployer.address)
    ]);

    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.bool(true));

    // Test getting boost info
    let boostInfo = chain.callReadOnlyFn(
      'scanbit',
      'get-boost-info',
      [types.uint(1)],
      deployer.address
    );

    let boostData = boostInfo.result.expectSome() as any;
    assertEquals(boostData['boost-multiplier'], types.uint(2));

    // Test getting current boost
    let currentBoost = chain.callReadOnlyFn(
      'scanbit',
      'get-current-boost',
      [types.uint(1)],
      deployer.address
    );

    assertEquals(currentBoost.result, types.uint(2));

    // Test scanning with boost - should get 2x reward (200 instead of 100)
    block = chain.mineBlock([
      Tx.contractCall('scanbit', 'scan-qr-code', [
        types.uint(1) // qr-id
      ], user1.address)
    ]);

    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.uint(200)); // 100 * 2 multiplier

    // Check user's token balance
    let balance = chain.callReadOnlyFn(
      'scanbit',
      'get-token-balance',
      [types.principal(user1.address)],
      user1.address
    );

    assertEquals(balance.result, types.uint(200));

    // Test deactivating boost
    block = chain.mineBlock([
      Tx.contractCall('scanbit', 'deactivate-boost', [
        types.uint(1) // qr-id
      ], deployer.address)
    ]);

    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.bool(true));

    // Test getting current boost after deactivation - should be 1
    currentBoost = chain.callReadOnlyFn(
      'scanbit',
      'get-current-boost',
      [types.uint(1)],
      deployer.address
    );

    assertEquals(currentBoost.result, types.uint(1));
  }
});

Clarinet.test({
  name: "Test boost expiry functionality", 
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;

    let block = chain.mineBlock([
      // Create a QR code
      Tx.contractCall('scanbit', 'create-qr-code', [
        types.uint(100), // reward amount
        types.uint(10),  // max scans
        types.uint(1000), // duration blocks
        types.ascii("Test Location"), // location
        types.ascii("Test Category")  // category
      ], deployer.address),
      // Activate boost with very short duration
      Tx.contractCall('scanbit', 'activate-boost', [
        types.uint(1), // qr-id
        types.uint(3), // multiplier (3x)
        types.uint(1) // duration in blocks (very short)
      ], deployer.address)
    ]);

    assertEquals(block.receipts.length, 2);
    assertEquals(block.receipts[0].result.expectOk(), types.uint(1));
    assertEquals(block.receipts[1].result.expectOk(), types.bool(true));

    // Mine some blocks to expire the boost
    chain.mineEmptyBlock(5);

    // Test getting current boost after expiry - should be 1
    let currentBoost = chain.callReadOnlyFn(
      'scanbit',
      'get-current-boost',
      [types.uint(1)],
      deployer.address
    );

    assertEquals(currentBoost.result, types.uint(1));
  }
});

Clarinet.test({
  name: "Test boost authorization",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const user1 = accounts.get('wallet_1')!;

    let block = chain.mineBlock([
      // Create a QR code with deployer
      Tx.contractCall('scanbit', 'create-qr-code', [
        types.uint(100), // reward amount
        types.uint(10),  // max scans
        types.uint(1000), // duration blocks
        types.ascii("Test Location"), // location
        types.ascii("Test Category")  // category
      ], deployer.address)
    ]);

    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.uint(1));

    // Try to activate boost as non-creator - should fail
    block = chain.mineBlock([
      Tx.contractCall('scanbit', 'activate-boost', [
        types.uint(1), // qr-id
        types.uint(2), // multiplier
        types.uint(100) // duration
      ], user1.address)
    ]);

    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectErr(), types.uint(100)); // ERR_NOT_AUTHORIZED
  }
});
