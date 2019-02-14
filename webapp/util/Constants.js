sap.ui.define([], function () {

	"use strict";

	var mConstants = {
		/**
		 *All Constant Strings that need to match the configuration file from the DQS
		 */
		 
		//Constants for faultyaddress
		
		//FROM BACKEND
		ShippingAddress : "ShippingAddress",
		City: "City",
		Postcode: "Postcode",
		Country: "Country",
		QualityLevel:"QualityLevel",		
		LOW:"LO",
		HIGH:"HI",
		
		//FROM DQS
		Shipping_Address : "SHIPPINGADDRESS",
		POSTCODE: "POSTCODE",
		COUNTRY: "COUNTRY",
		CITY: "CITY",
		SuggestionErrorCode: "Suggestion Error Code",
		SuggestionCount: "Suggestion Count",
		ADDR_INFO_CODE:"ADDR_INFO_CODE",
		DateOfValidation:"DateofValidation",
		ProposalAccepted:"ProposalAccepted",
		DQMResponse:"DQMResponse",

		//CONSTANTS FOR SUGGESTIONS
		
		//FROM BACKEND
		AddressDelivery:"AddressDelivery",
		Selection:"Selection",
		SingleAddress:"Singleaddress",
		RangeType:"Rangetype",
		SecSideIndicator:"Secsideindicator",
		PrimSideIndicator:"Primsideindicator",
		
		//FROM DQS
		sugg_addr_address_delivery:"sugg_addr_address_delivery",
		sugg_addr_selection:"sugg_addr_selection",
		sugg_addr_single_address:"sugg_addr_single_address",
		sugg_addr_range_type:"sugg_addr_range_type",
		sugg_addr_sec_side_indicator:"sugg_addr_sec_side_indicator",
		sugg_addr_prim_side_indicator:"sugg_addr_prim_side_indicator"
	};

	return Object.freeze(mConstants);

});
