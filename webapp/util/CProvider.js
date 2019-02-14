sap.ui.define([
  "com/sap/build/sap/faultyAddresses/util/Constants"
], function (mConstants) {
  "use strict";

  function CProvider(mConstants) {
    this._mConstants = mConstants;
  }

  CProvider.prototype.getStr = function(sConstant) {
    var sString = this._mConstants[sConstant];

    if (!sString) {
      throw new Error("String defined by constant '" + sConstant + "' not found in CProvider.js");
    }

    return sString;
  };
  return new CProvider(mConstants);
});