sap.ui.define(["sap/ui/core/mvc/Controller",
	"sap/m/MessageBox",
	"sap/ui/core/routing/History",
	"sap/m/MessageToast",
	"com/sap/build/sap/faultyAddresses/connector/DQSConnector",
	"com/sap/build/sap/faultyAddresses/util/CProvider"
], function(BaseController, MessageBox, History, MessageToast, DQSConnector, CProvider) {
	"use strict";

	return BaseController.extend("com.sap.build.sap.faultyAddresses.controller.EditPage", {
		handleRouteMatched: function(oEvent) {

			var oParams = {};
			console.log("routematch: " + oEvent.mParameters.data.context);
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
		_onSavePress: function(oEvent) {
			var oModel = this.getView().getModel();
			
			var oEntry = {};
			oEntry[CProvider.getStr("ShippingAddress")] = ( sap.ui.getCore().byId("__component0---Detail--attrAddress") ? sap.ui.getCore().byId("__component0---Detail--attrAddress").getText() : "");
			oEntry[CProvider.getStr("Postcode")] = ( sap.ui.getCore().byId("__component0---Detail--attrPostcode") ? sap.ui.getCore().byId("__component0---Detail--attrPostcode").getText() : "");
			oEntry[CProvider.getStr("Country")] = ( sap.ui.getCore().byId("__component0---Detail--attrCountry") ? sap.ui.getCore().byId("__component0---Detail--attrCountry").getText() : "");
			oEntry[CProvider.getStr("City")] = ( sap.ui.getCore().byId("__component0---Detail--attrCity") ? sap.ui.getCore().byId("__component0---Detail--attrCity").getText() : "");
			oModel.update("/" + this.sContext, oEntry, null, oModel.refresh());

			var oBindingContext = oEvent.getSource().getBindingContext();

			return new Promise(function(fnResolve) {

			this.doNavigate("Detail", oBindingContext, fnResolve, "");
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
		_onCancelPress: function(oEvent) {
			this.getView().getModel().refresh();
			var oBindingContext = oEvent.getSource().getBindingContext();

			return new Promise(function(fnResolve) {

				this.doNavigate("Detail", oBindingContext, fnResolve, "");
			}.bind(this)).catch(function(err) {
				if (err !== undefined) {
					MessageBox.error(err.message);
				}
			});

		},
		onInit: function() {
			this.connector = new DQSConnector();
			this.mBindingOptions = {};
			this.oRouter = sap.ui.core.UIComponent.getRouterFor(this);
			this.oRouter.getTarget("EditPage").attachDisplay(jQuery.proxy(this.handleRouteMatched, this));
			var oView = this.getView();
			
			oView.addEventDelegate({
				onBeforeShow: function() {
					if (sap.ui.Device.system.phone) {
						var oPage = oView.getContent()[0];
						if (oPage.getShowNavButton && !oPage.getShowNavButton()) {
							oPage.setShowNavButton(true);
							oPage.attachNavButtonPress(function() {
								this.oRouter.navTo("Detail", {}, true);
							}.bind(this));
						}
					}
				}.bind(this)
			});
		
		}
	});
}, /* bExport= */ true);