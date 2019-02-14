sap.ui.define([
  "sap/ui/model/resource/ResourceModel",
  "sap/ui/model/json/JSONModel",
  "com/sap/build/sap/faultyAddresses/util/CProvider"
], function (ResourceModel, JSONModel, CProvider) {
  "use strict";

  return sap.ui.base.Object.extend("com.sap.build.sap.faultyAddresses.connector.DQSConnector", {

    _sDefaultContentType: "application/json",
    _sDefaultDataType: "json",
    _oI18n: new ResourceModel({bundleUrl:"./assets/i18n/i18n.properties"}).getResourceBundle(),

	doAddressValidation: function(sAddress, sCountry, sPostcode, sCity, aSuggestionReply) {
    	var oDeferred = $.Deferred();
		this.prepareData(sAddress, sCountry, sPostcode, sCity, aSuggestionReply).then(function(){
			this.oResult = new JSONModel({});
			jQuery.ajax({
				type: "POST",
				url: "/DQaaS/addressCleanse",
				data: this.oRequestModel.getJSON(),
				contentType: "application/json",
				dataType: "json",
				success: function (data) {
					this.oResult.setJSON(JSON.stringify(data));
					oDeferred.resolve(this.oResult);
				}.bind(this)
            });	
		}.bind(this));
		return oDeferred.promise();
    },
    
    prepareData: function(sAddress, sCountry, sPostcode, sCity, aSuggestionReply){
		var oDeferred = $.Deferred();
		this.oRequestModel = new JSONModel();
		var oAddressInput = {};
		oAddressInput[CProvider.getStr("Shipping_Address")] = sAddress;
		oAddressInput[CProvider.getStr("COUNTRY")] =  sCountry;
		oAddressInput[CProvider.getStr("POSTCODE")] = sPostcode;
		oAddressInput[CProvider.getStr("CITY")] = sCity;
		this.oRequestModel.setProperty("/addressInput", oAddressInput);
		this.oRequestModel.setProperty("/configurationName", "AddressValidation_Suggestions");
		this.oRequestModel.setProperty("/suggestionReply", aSuggestionReply);
		oDeferred.resolve();
		
		return oDeferred.promise();
    }
  });
});
