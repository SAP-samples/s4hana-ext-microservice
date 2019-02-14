sap.ui.define(["sap/ui/core/mvc/Controller",
	"sap/m/MessageBox",
	"sap/ui/core/routing/History",
	"com/sap/build/sap/faultyAddresses/util/CProvider",
		"sap/ui/model/Filter",
	"sap/ui/model/FilterOperator"
], function(BaseController, MessageBox, History, CProvider, Filter, FilterOperator) {
	"use strict";

	return BaseController.extend("com.sap.build.sap.faultyAddresses.controller.Master", {
		formatQualityLevel: function(sQualityLevel){
			if(sQualityLevel === CProvider.getStr("HIGH") ){
				return "High";
			}else {
				return "Low";
			}	
		},
		
		handleFilterchange: function(oEvent){
			this._filter();	
		},
		
		handleRouteMatched: function(oEvent) {

			var oParams = {};
			var oView = this.getView();
			var bSelectFirstListItem = true;
			if (oEvent.mParameters.data.context || oEvent.mParameters.data.masterContext) {
				this.sContext = oEvent.mParameters.data.context;
				var oPath;
				this.sMasterContext = oEvent.mParameters.data.masterContext;
				if (this.sMasterContext) {
					oPath = {
						path: "/" + this.sMasterContext,
						parameters: oParams
					};
					this.getView().bindObject(oPath);
				} else if (this.sContext) {
					var sCurrentContextPath = "/" + this.sContext;
					this._attachSelectListItemWithContextPath(sCurrentContextPath);
					bSelectFirstListItem = false;
					}
			this._filter();	
			}
			if (bSelectFirstListItem) {
				oView.addEventDelegate({
					onBeforeShow: function() {
						var oContent = this.getView().getContent();
						if (oContent) {
							if (!sap.ui.Device.system.phone) {
								var oList = oContent[0].getContent() ? oContent[0].getContent()[0] : undefined;
								if (oList) {
									var sContentName = oList.getMetadata().getName();
									if (sContentName.indexOf("List") > -1) {
										oList.attachEventOnce("updateFinished", function() {
											var oFirstListItem = this.getItems()[0];
											if (oFirstListItem) {
												oList.setSelectedItem(oFirstListItem);
												oList.fireItemPress({
													listItem: oFirstListItem
												});
											}
										}.bind(oList));
									}
								}
							}
						}
					}.bind(this)
				});
			}

		},
		_attachSelectListItemWithContextPath: function(sContextPath) {
			var oView = this.getView();
			var oContent = this.getView().getContent();
			if (oContent) {
				if (!sap.ui.Device.system.phone) {
					var oList = oContent[0].getContent() ? oContent[0].getContent()[0] : undefined;
					if (oList && sContextPath) {
						var sContentName = oList.getMetadata().getName();
						var oItemToSelect, oItem, oContext, aItems, i;
						if (sContentName.indexOf("List") > -1) {
							if (oList.getItems().length) {
								oItemToSelect = null;
								aItems = oList.getItems();
								for (i = 0; i < aItems.length; i++) {
									oItem = aItems[i];
									oContext = oItem.getBindingContext();
									if (oContext && oContext.getPath() === sContextPath) {
										oItemToSelect = oItem;
									}
								}
								if (oItemToSelect) {
									oList.setSelectedItem(oItemToSelect);
								}
							} else {
								oView.addEventDelegate({
									onBeforeShow: function() {
										oList.attachEventOnce("updateFinished", function() {
											oItemToSelect = null;
											aItems = oList.getItems();
											for (i = 0; i < aItems.length; i++) {
												oItem = aItems[i];
												oContext = oItem.getBindingContext();
												if (oContext && oContext.getPath() === sContextPath) {
													oItemToSelect = oItem;
												}
											}
											if (oItemToSelect) {
												oList.setSelectedItem(oItemToSelect);
											}
										});
									}
								});
							}
						}

					}
				}
			}

		},
		_onListItemPress: function(oEvent) {

			var oBindingContext = oEvent.getParameter("listItem").getBindingContext();

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
			this._filter();
		},
		_onObjectListItemPress: function(oEvent) {

			var oBindingContext = oEvent.getSource().getBindingContext();

			return new Promise(function(fnResolve) {

				this.doNavigate("Detail", oBindingContext, fnResolve, "");
			}.bind(this)).catch(function(err) {
				if (err !== undefined) {
					MessageBox.error(err.message);
				}
			});
		},
		
		_filter: function() {
			var oFilter = new Filter("QualityLevel", FilterOperator.EQ, this._oComboBox.getSelectedKey());
			var oBinding = this._oList.getBinding("items");
			oBinding.filter(null);
			oBinding.filter(oFilter);
		},
		
		onInit: function() {
			this._oComboBox = this.getView().byId("ComboBox");
			this._oList = this.getView().byId("MasterList");
			
			this.mBindingOptions = {};
			this.oRouter = sap.ui.core.UIComponent.getRouterFor(this);
			this.oRouter.getTarget("Master").attachDisplay(jQuery.proxy(this.handleRouteMatched, this));
		}
	});
}, /* bExport= */ true);
