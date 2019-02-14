sap.ui.define(["sap/ui/core/mvc/Controller",
	"sap/m/MessageBox",
	"sap/ui/core/routing/History",
	"com/sap/build/sap/faultyAddresses/connector/DQSConnector",
	"sap/ui/model/json/JSONModel",
	"sap/m/Dialog",
	"sap/m/MessageToast",
	"com/sap/build/sap/faultyAddresses/util/CProvider"
], function(BaseController, MessageBox, History, DQSConnector, JSONModel, Dialog, MessageToast, CProvider) {
	"use strict";

	return BaseController.extend("com.sap.build.sap.faultyAddresses.controller.Detail", {
		handleRouteMatched: function(oEvent) {

			var oParams = {};

			if (oEvent.mParameters.data.context) {
				this.sContext = oEvent.mParameters.data.context;
				var oPath;
				if (this.sContext) {
					oPath = {
						path: "/" + this.sContext,
						parameters: oParams
					};
					this.getView().bindObject(oPath);
				}
			}
		},
		
		_onAccept: function() {
			var oModel = this.getView().getModel();
			var oCurrentAddress = oModel.oData[this.sContext];

			if(sap.ui.getCore().byId("__component0---Detail--suggestionList").getSelectedItem()){
				var oSelectedSuggestion = oModel.getObject(sap.ui.getCore().byId("__component0---Detail--suggestionList").getSelectedItem().getBindingContext().sPath);
				var iSuggetionNumber = oSelectedSuggestion.Selection;
				var oInput = sap.ui.getCore().byId("__component0---Detail--suggestionList").getSelectedItem().getContent()[0];
				var iAdditionalInfo = oInput.getValue();
			}else{
				MessageToast.show("Please select a suggestion from the list!");
				return;
			}
			
			if (oInput.getVisible() === true && !iAdditionalInfo){
				oInput.setValueStateText("Enter number");
			  	oInput.setValueState("Error");
			  	return;
			}
			
			this.SuggestionModel.setProperty("/suggestionReply/", [iSuggetionNumber]);			
			if (iAdditionalInfo) {
				this.SuggestionModel.getProperty("/suggestionReply/").push(iAdditionalInfo);
			}

			this.oDQSConnector.doAddressValidation(oCurrentAddress[CProvider.getStr("ShippingAddress")], oCurrentAddress[CProvider.getStr("Country")], oCurrentAddress[CProvider.getStr("Postcode")], oCurrentAddress[CProvider.getStr("City")], this.SuggestionModel.getProperty("/suggestionReply"))
			  .then(function (oResult){
			  	var sSuggestionError = oResult.getProperty("/" + CProvider.getStr("SuggestionErrorCode"));
			  	var iSuggestionCount = oResult.getProperty("/" + CProvider.getStr("SuggestionCount"));
			  	if (sSuggestionError === "3" || sSuggestionError === "4" ||  sSuggestionError === "5") {
			  		oInput.setValueStateText("Enter number within Range");
			  		oInput.setValueState("Error");
			  	} else {
				  	if ( iSuggestionCount === null || iSuggestionCount === 0 ) {
						this.deleteSuggestions(oModel);
						if (oResult.getProperty("/" + CProvider.getStr("ADDR_INFO_CODE")) === ""){
							this.updateFaultyAddressFromResult(oResult, this.sContext, CProvider.getStr("HIGH"), oModel);
						}else {
							if ( (oResult.getProperty("/" + CProvider.getStr("POSTCODE")) === "") && (oResult.getProperty("/" + CProvider.getStr("CITY")) === "") && (oResult.getProperty("/" + CProvider.getStr("Shipping_Address")) === "") ){
								this.updateFaultyAddressFromInput(this.sContext, CProvider.getStr("LOW"), oModel);
							}else{
								this.updateFaultyAddressFromResult(oResult, this.sContext, CProvider.getStr("LOW"), oModel);
							}
						}
					}else {
						this.prepareSuggestionsFromResult(oResult);
						this.SuggestionModel.refresh();
						this.ConfirmationDialog.open();
					}
					}
			  	}.bind(this));
			},
		
		handleSave: function() {
			var oModel = this.getView().getModel();
			var oCurrentAddress = oModel.oData[this.sContext];

			if(sap.ui.getCore().byId("suggestionReplyList").getSelectedItem()){
				var iSuggetionNumber = parseInt(sap.ui.getCore().byId("suggestionReplyList").getSelectedItem().getBindingContext().sPath.split("/").pop()) + 1;
				var oInput = sap.ui.getCore().byId("suggestionReplyList").getSelectedItem().getContent()[0];
				var iAdditionalInfo = oInput.getValue();
			}else{
				MessageToast.show("Please select a suggestion from the list!");
				return;
			}
			
			if (oInput.getVisible() === true && !iAdditionalInfo){
				oInput.setValueStateText("Enter number");
			  	oInput.setValueState("Error");
			  	return;
			}
			
			this.SuggestionModel.getProperty("/suggestionReply").push(iSuggetionNumber);			
			if (iAdditionalInfo) {
				this.SuggestionModel.getProperty("/suggestionReply").push(iAdditionalInfo);
			}
			this.oDQSConnector.doAddressValidation(oCurrentAddress[CProvider.getStr("ShippingAddress")], oCurrentAddress[CProvider.getStr("Country")], oCurrentAddress[CProvider.getStr("Postcode")], oCurrentAddress[CProvider.getStr("City")], this.SuggestionModel.getProperty("/suggestionReply"))
			  .then(function (oResult){
			  	var sSuggestionError = oResult.getProperty("/" + CProvider.getStr("SuggestionErrorCode"));
			  	var iSuggestionCount = oResult.getProperty("/" + CProvider.getStr("SuggestionCount"));
			  	if (sSuggestionError === "3" || sSuggestionError === "4" ||  sSuggestionError === "5") {
			  		oInput.setValueStateText("Enter number within Range");
			  		oInput.setValueState("Error");
			  	} else {
				  	if (iSuggestionCount === null || iSuggestionCount === 0 ) {
						this.deleteSuggestions(oModel);
						if (oResult.getProperty("/" + CProvider.getStr("ADDR_INFO_CODE")) === ""){
							this.updateFaultyAddressFromResult(oResult, this.sContext, CProvider.getStr("HIGH"), oModel, true);
							this.ConfirmationDialog.close();
						}else {
							if ( (oResult.getProperty("/" + CProvider.getStr("POSTCODE")) === "") && (oResult.getProperty("/" + CProvider.getStr("CITY")) === "") && (oResult.getProperty("/" + CProvider.getStr("Shipping_Address")) === "") ){
								this.updateFaultyAddressFromInput(this.sContext, CProvider.getStr("LOW"), oModel);
							}else{
								this.updateFaultyAddressFromResult(oResult, this.sContext, CProvider.getStr("LOW"), oModel);
							}
							this.ConfirmationDialog.close();
						}
					}else {
						this.prepareSuggestionsFromResult(oResult);
						this.SuggestionModel.refresh();
					}
					}
			  	}.bind(this));
			},
		
		prepareSuggestionsFromResult: function(oResult){
			var aSuggestions = oResult.getProperty("/Suggestion List");
			this.SuggestionModel.setProperty("/Suggestions",[]);
			for (var i = 0 ; i < aSuggestions.length ; i++){
				var oSuggestion = {};
				oSuggestion[CProvider.getStr("AddressDelivery")] = aSuggestions[i][CProvider.getStr("sugg_addr_address_delivery")] ;
				oSuggestion[CProvider.getStr("Selection")] = aSuggestions[i][CProvider.getStr("sugg_addr_selection")].toString();
				oSuggestion[CProvider.getStr("SingleAddress")] = aSuggestions[i][CProvider.getStr("sugg_addr_single_address")];
				oSuggestion[CProvider.getStr("RangeType")] = aSuggestions[i][CProvider.getStr("sugg_addr_range_type")];
				oSuggestion[CProvider.getStr("SecSideIndicator")] = aSuggestions[i][CProvider.getStr("sugg_addr_sec_side_indicator")];
				oSuggestion[CProvider.getStr("PrimSideIndicator")] = aSuggestions[i][CProvider.getStr("sugg_addr_prim_side_indicator")];
				this.SuggestionModel.getProperty("/Suggestions").push(oSuggestion);
			}
		},
		
		updateFaultyAddressFromResult: function(oResult, sContext, sQualityLevel, oModel, bAcceptProposal){
			var oEntry = {};
			oEntry[CProvider.getStr("ShippingAddress")] = oResult.getProperty("/" + CProvider.getStr("Shipping_Address"));
			oEntry[CProvider.getStr("Postcode")]  = oResult.getProperty("/" + CProvider.getStr("POSTCODE"));
			oEntry[CProvider.getStr("Country")]  = oResult.getProperty("/" + CProvider.getStr("COUNTRY"));
			oEntry[CProvider.getStr("City")] = oResult.getProperty("/" + CProvider.getStr("CITY"));
			oEntry[CProvider.getStr("QualityLevel")]  = sQualityLevel;
			if (bAcceptProposal){
				oEntry[CProvider.getStr("ProposalAccepted")] = true;
				oEntry[CProvider.getStr("DQMResponse")] = "";
			}
			oModel.update("/" + sContext, oEntry, null, oModel.refresh());
		},
		
		updateFaultyAddressFromInput: function(sContext, sQualityLevel, oModel){
			var oEntry = {};
			oEntry[CProvider.getStr("ShippingAddress")] = (sap.ui.getCore().byId("__component0---Detail--attrAddress") ? sap.ui.getCore().byId("__component0---Detail--attrAddress").getText() : "");
			oEntry[CProvider.getStr("Postcode")]  = ( sap.ui.getCore().byId("__component0---Detail--attrPostcode") ? sap.ui.getCore().byId("__component0---Detail--attrPostcode").getText() : "");
			oEntry[CProvider.getStr("Country")]  = ( sap.ui.getCore().byId("__component0---Detail--attrCountry") ? sap.ui.getCore().byId("__component0---Detail--attrCountry").getText() : "");
			oEntry[CProvider.getStr("City")]  = ( sap.ui.getCore().byId("__component0---Detail--attrCity") ? sap.ui.getCore().byId("__component0---Detail--attrCity").getText() : "");
			oEntry[CProvider.getStr("QualityLevel")]  = sQualityLevel;
			
			oModel.update("/" + sContext, oEntry, null, oModel.refresh() );
		},
		
		deleteSuggestions: function (oModel){
			sap.ui.getCore().byId("__component0---Detail--suggestionList").getItems().forEach(function(oItem) {
				oModel.remove(oItem.getBindingContext().sPath);
			});
		},
		
		createSuggestionsFromResult: function (oResult, sContext, oModel){
			var aSuggestions = oResult.getProperty("/Suggestion List/");
			aSuggestions.forEach(function (oItem){
				var oSuggestion = {};
				oSuggestion[CProvider.getStr("AddressDelivery")] = oItem[CProvider.getStr("sugg_addr_address_delivery")] ;
				oSuggestion[CProvider.getStr("Selection")] = oItem[CProvider.getStr("sugg_addr_selection")].toString();
				oSuggestion[CProvider.getStr("SingleAddress")] = oItem[CProvider.getStr("sugg_addr_single_address")];
				oSuggestion[CProvider.getStr("RangeType")] = oItem[CProvider.getStr("sugg_addr_range_type")];
				oSuggestion[CProvider.getStr("SecSideIndicator")] = oItem[CProvider.getStr("sugg_addr_sec_side_indicator")];
				oSuggestion[CProvider.getStr("PrimSideIndicator")] = oItem[CProvider.getStr("sugg_addr_prim_side_indicator")];
				oModel.create("/" + sContext + "/to_Suggestion", oSuggestion);
			});	
		},

		formatDQMResponse: function(sDQMResponse){
			if(sDQMResponse === ""){
				return "No Message from DQM";
			} else {
				return sDQMResponse;
			}
		},
		formatAccept: function(sQualityLevel){
			if(sQualityLevel === CProvider.getStr("HIGH")){
				return false;
			}
			return true;
		},
		
		formatDelete: function(sQualityLevel){
			if(sQualityLevel === CProvider.getStr("HIGH")){
				return true;
			}
			return false;
		},
		
		formatPlaceholder: function(oSuggestion){
			switch (oSuggestion[CProvider.getStr("RangeType")]){
				case "PRIM":
					if (oSuggestion[CProvider.getStr("PrimSideIndicator")] === "E"){
						return "Even House Number";	
					}else{
						if (oSuggestion[CProvider.getStr("PrimSideIndicator")] === "O"){
							return "Odd House Number";
						}else{
							return "House Number";
						}
					}
					break;
				case "FLOOR":
					if (oSuggestion[CProvider.getStr("SecSideIndicator")] === "E"){
						return "Even Floor Number";	
					}else{
						if (oSuggestion[CProvider.getStr("SecSideIndicator")] === "O"){
							return "Odd Floor Number";
						}else{
							return "Floor Number";
						}
					}
					break;
				case "UNIT":
					if (oSuggestion[CProvider.getStr("SecSideIndicator")] === "E"){
						return "Even Unit Number";	
					}else{
						if (oSuggestion[CProvider.getStr("SecSideIndicator")] === "O"){
							return "Odd Unit Number";
						}else{
							return "Unit Number";
						}
					}
					break;
				default:
					return "";
			}
		},
		
		formatInputVisible: function(oSuggestion){
			if (oSuggestion[CProvider.getStr("RangeType")] === ""){
				return false;
			}
			return true;
		},
		
		handleCancel: function(){
			this.ConfirmationDialog.close();
		},
		_onDelete: function(){
			var oModel = this.getView().getModel();
			oModel.remove("/" + this.sContext, null, oModel.refresh());
		},
		
		_onEdit: function(oEvent) {

			var oBindingContext = oEvent.getSource().getBindingContext();

			return new Promise(function(fnResolve) {

				this.doNavigate("EditPage", oBindingContext, fnResolve, "");
			}.bind(this)).catch(function(err) {
				if (err !== undefined) {
					MessageBox.error(err.message);
				}
			});
		},
		doNavigate: function(sRouteName, oBindingContext, fnPromiseResolve, sViaRelation) {

			var sPath = (oBindingContext) ? oBindingContext.getPath() : null;
			var oModel = (oBindingContext) ? oBindingContext.getModel() : null;

			var sEntityNameSet;
			if (sPath !== null && sPath !== "") {
				if (sPath.substring(0, 1) === "/") {
					sPath = sPath.substring(1);
				}
				sEntityNameSet = sPath.split("(")[0];
			}
			var sNavigationPropertyName;
			var sMasterContext = this.sMasterContext ? this.sMasterContext : sPath;

			if (sEntityNameSet !== null) {
				sNavigationPropertyName = sViaRelation || this.getOwnerComponent().getNavigationPropertyForNavigationWithContext(sEntityNameSet,
					sRouteName);
			}
			if (sNavigationPropertyName !== null && sNavigationPropertyName !== undefined) {
				if (sNavigationPropertyName === "") {
					this.oRouter.navTo(sRouteName, {
						context: sPath,
						masterContext: sMasterContext
					}, false);
				} else {
					oModel.createBindingContext(sNavigationPropertyName, oBindingContext, null, function(bindingContext) {
						if (bindingContext) {
							sPath = bindingContext.getPath();
							if (sPath.substring(0, 1) === "/") {
								sPath = sPath.substring(1);
							}
						} else {
							sPath = "undefined";
						}

						// If the navigation is a 1-n, sPath would be "undefined" as this is not supported in Build
						if (sPath === "undefined") {
							this.oRouter.navTo(sRouteName);
						} else {
							this.oRouter.navTo(sRouteName, {
								context: sPath,
								masterContext: sMasterContext
							}, false);
						}
					}.bind(this));
				}
			} else {
				this.oRouter.navTo(sRouteName);
			}

			if (typeof fnPromiseResolve === "function") {
				fnPromiseResolve();
			}
			
		},
		onInit: function() {
			this.aSelectedSuggestion = [];
			this.SuggestionModel = new JSONModel();
			this.getView().setModel(this.SuggestionModel, "SuggestionReply");
			
			if (!this.ConfirmationDialog) {
		        this.ConfirmationDialog = sap.ui.xmlfragment("com.sap.build.sap.faultyAddresses.dialogs.ConfirmationDialog", this);
		        this.getView().addDependent(this.ConfirmationDialog);
		        this.ConfirmationDialog.setModel(this.SuggestionModel);
	    	} 
			this.oDQSConnector = new DQSConnector();
			this.mBindingOptions = {};
			this.oRouter = sap.ui.core.UIComponent.getRouterFor(this);
			this.oRouter.getTarget("Detail").attachDisplay(jQuery.proxy(this.handleRouteMatched, this));
			var oView = this.getView();
			oView.addEventDelegate({
				onBeforeShow: function() {
					if (sap.ui.Device.system.phone) {
						var oPage = oView.getContent()[0];
						if (oPage.getShowNavButton && !oPage.getShowNavButton()) {
							oPage.setShowNavButton(true);
							oPage.attachNavButtonPress(function() {
								this.oRouter.navTo("Master", {}, true);
							}.bind(this));
						}
					}
				}.bind(this)
			});

		}
	});
}, /* bExport= */ true);