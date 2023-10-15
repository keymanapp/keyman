/*
 * This somewhat peculiar function is used in `build.sh configure` when
 * precompiling the validators and makes it possible to use the extended formats
 * in ajv-formats.
 */
function formats(ajv) {
  require("ajv-formats")(ajv);
}

module.exports = formats;
