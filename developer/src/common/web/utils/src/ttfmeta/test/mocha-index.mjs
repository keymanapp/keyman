import "mocha";
import fs from "fs";
import assert from "assert";
import ttfMeta from "../index.mjs";

const fontFile = "./assets/font/Myanmar3.ttf";

describe("ttfMeta", () => {
  it("Using callback", () => {
    ttfMeta.ttfInfo(fontFile, function(err, info) {
      assert.strictEqual(null, err);

      // assert.strictEqual(7,Object.keys(info.tables.name).length);
      assert.strictEqual(9, Object.keys(info.tables.post).length);
      assert.strictEqual(2, Object.keys(info.tables.os2).length);
    });
  });

  it("Using promise", () => {
    ttfMeta.promise(fontFile).then(function(info) {
      // assert.strictEqual(7,Object.keys(info.tables.name).length);
      assert.strictEqual(9, Object.keys(info.tables.post).length);
      assert.strictEqual(2, Object.keys(info.tables.os2).length);
    });
  });

  it("Returning object has meta property", () => {
    ttfMeta.promise(fontFile).then(function(info) {
      assert.strictEqual(4, Object.keys(info.meta).length);

      assert.ok(info.meta.property);
      assert.ok(info.meta.license);
      assert.ok(info.meta.reference);
      assert.ok(info.meta.description);
    });
  });

  it("Expecting error on ttfMeta.ttfInfo callback", () => {
    ttfMeta.ttfInfo("./test/none.ttf", function(error) {
      assert.ok(error);
      assert.strictEqual("string", typeof error);
    });
  });

  it("Expecting error on ttfMeta.promise", () => {
    ttfMeta
      .promise("./test/none.ttf")
      .then(function(info) {
        assert.throws(info);
      })
      .catch(function(error) {
        assert.ok(error);
        assert.strictEqual("string", typeof error);
      });
  });

  it("Reading from Buffer", () => {
    fs.readFile(fontFile, function(err, buffer) {
      if (err) {
        throw err;
      } else {
        ttfMeta
          .promise(buffer)
          .then(function(info) {
            assert.ok(info);
          })
          .catch(function(error) {
            throw error;
          });
      }
    });
  });
});
