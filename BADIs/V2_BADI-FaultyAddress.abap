* After Modify Determination for Node ID FAULTYADDRESS
*
* Importing Parameter : association (Navigation to Parent/Child/Associated Node Instances)
*                       write (API for creating and updating Custom Business Object Node Instances)
* Changing Parameter  : FAULTYADDRESS (Current Node Data)
DATA:   lv_customerrecord           TYPE yy1_customerrecord,
        write_api_customerrecord    TYPE REF TO IF_CBO_WRITE_NODE,
        tsl                         TYPE timestampl,
		lv_customerrecord_key		TYPE yy1_kr_customerrecord.

GET TIME STAMP FIELD tsl.

if ( tsl - faultyaddress-dateofvalidation ) > 5.

	lv_customerrecord_key = faultyaddress-addressid.
    write_api_customerrecord = write->get_root(
    	business_object_id  = 'YY1_CUSTOMERRECORD'
    	key                 = lv_customerrecord_key
    ).
    write_api_customerrecord->get(
    	IMPORTING data = lv_customerrecord
    ).

	lv_customerrecord-postcode = faultyaddress-postcode.
	lv_customerrecord-shippingaddress = faultyaddress-shippingaddress.
	lv_customerrecord-country = faultyaddress-country.
	lv_customerrecord-city = faultyaddress-city.
	lv_customerrecord-dqmresponse = faultyaddress-dqmresponse.
	lv_customerrecord-qualitylevel = faultyaddress-qualitylevel.

	write_api_customerrecord->update(
		CHANGING data = lv_customerrecord
	).
    if faultyaddress-proposalaccepted = abap_false.
        write_api_customerrecord->get(
        	IMPORTING data = lv_customerrecord
        ).

        faultyaddress-qualitylevel = lv_customerrecord-qualitylevel.
        faultyaddress-city = lv_customerrecord-city.
        faultyaddress-country = lv_customerrecord-country.
        faultyaddress-postcode = lv_customerrecord-postcode.
        faultyaddress-shippingaddress = lv_customerrecord-shippingaddress.
        faultyaddress-dateofvalidation = tsl.
        faultyaddress-dqmresponse = lv_customerrecord-dqmresponse.
    ENDIF.
    faultyaddress-proposalaccepted = abap_false.
ENDIF.
