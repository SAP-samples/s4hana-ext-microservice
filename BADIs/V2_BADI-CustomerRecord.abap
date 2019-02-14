* After Modify Determination for Node ID customerrecord
*
* Importing Parameter : association (Navigation to Parent/Child/Associated Node Instances)
*                       write (API for creating and updating Custom Business Object Node Instances)
* Changing Parameter  : CUSTOMERRECORD (Current Node Data)

* Constant Strings coming from DQS. These should be the same as in the DQS Configuration *
DATA:   dqs_ShippingAddress TYPE string VALUE 'SHIPPINGADDRESS',
        dqs_country         TYPE string VALUE 'COUNTRY',
        dqs_city            TYPE string VALUE 'CITY',
        dqs_postcode        TYPE string VALUE 'POSTCODE',
        dqs_addrinfocode    TYPE string VALUE 'ADDR_INFO_CODE',
        dqs_configname      TYPE string VALUE 'AddressValidation_Suggestions',
        dqs_suggestioncount TYPE string VALUE 'Suggestion Count',
        dqs_sugg_selection  TYPE string VALUE 'sugg_addr_selection',
        dqs_sugg_addrdeli   TYPE string VALUE 'sugg_addr_address_delivery',
        dqs_sugg_singleaddr TYPE string VALUE 'sugg_addr_single_address',
        dqs_sugg_rangetype  TYPE string VALUE 'sugg_addr_range_type',
        dqs_sugg_lastline   TYPE string VALUE 'sugg_addr_lastline',
        dqs_sugg_secindica  TYPE string VALUE 'sugg_addr_sec_side_indicator',
        dqs_sugg_primindica TYPE string VALUE 'sugg_addr_prim_side_indicator'.

************** Prevaluation testing *********************
DATA: tsl              			TYPE timestampl,
      write_api_faultyaddress  	TYPE REF TO IF_CBO_WRITE_NODE,
	  faultyaddresscount		TYPE i VALUE 1,
	  ls_faultyaddress			TYPE yy1_faultyaddress,
	  lv_faultyaddress_key		TYPE yy1_kr_faultyaddress.

lv_faultyaddress_key = customerrecord-orderid.

*Get unchanged data from Database
SELECT *
	FROM YY1_customerrecord
    WHERE orderid = @customerrecord-orderid
    INTO @DATA(ls_customerrecord).
ENDSELECT.

*if the address was not changed the BADI should end.
IF ( ( ls_customerrecord-city EQ customerrecord-city ) AND
    ( ls_customerrecord-postcode EQ customerrecord-postcode ) AND
    ( ls_customerrecord-country EQ customerrecord-country ) AND
    ( ls_customerrecord-shippingaddress EQ customerrecord-shippingaddress ) ).
    EXIT.
ENDIF.

*Check if address just was validated
GET TIME STAMP FIELD tsl.
TRY.
	write_api_faultyaddress = write->get_root(
		business_object_id = 'YY1_FAULTYADDRESS'
		key                =  lv_faultyaddress_key
	).

	write_api_faultyaddress->get(
        IMPORTING data = ls_faultyaddress
    ).
*if the last valuation was done under 3 seconds ago, BADI will end
	IF ( tsl - ls_faultyaddress-dateofvalidation ) < 4.
		EXIT.
	ENDIF.

	IF ls_faultyaddress-proposalaccepted = abap_true.
    	ls_faultyaddress-proposalaccepted = abap_false.
	    write_api_faultyaddress->update(
            CHANGING
                data    = ls_faultyaddress
        ).
	    EXIT.
	ENDIF.
CATCH CX_CBO_WRITE_NOT_EXISTING.
	faultyaddresscount = 0.
ENDTRY.

************** SET CONNECTION TO DQS *********************
DATA: lt_headerparam        TYPE tihttpnvp,
      ls_header             TYPE LINE OF tihttpnvp,
      lv_body_post          TYPE string,
      lo_client             TYPE REF TO if_ble_http_client,
      requestpost           TYPE REF TO if_ble_http_request,
      lv_available    		TYPE abap_bool.

*Check if connection is available
cl_ble_http_client=>is_service_available(
  EXPORTING
    communication_scenario = 'YY1_CUSTOMERRECORD'
    outbound_service       = 'YY1_DQS_REST'
  RECEIVING
	available              = lv_available
).

IF NOT lv_available = abap_true.
    customerrecord-dqmresponse = 'Service seems to be unavailable, please check the communication scenario.'.
    EXIT.
ENDIF.

lo_client = cl_ble_http_client=>create(
    communication_scenario = 'YY1_CUSTOMERRECORD'
    outbound_service       = 'YY1_DQS_REST'
).

*construct JSON message from user input
lv_body_post =  |\{  "addressInput": \{| &
                |"{ dqs_ShippingAddress }": "{ customerrecord-shippingaddress } ",| &
                |"{ dqs_country }": "{ customerrecord-country }",| &
                |"{ dqs_postcode }": "{ customerrecord-postcode }",| &
                |"{ dqs_city }": "{ customerrecord-city }"  \},| &
                |"configurationName": "{ dqs_configname }"\} |.

*Add header values
CLEAR lt_headerparam.

ls_header-name = 'Accept'.
ls_header-value = 'application/json'.

APPEND ls_header TO lt_headerparam.

ls_header-name = 'Content-Type'.
ls_header-value = 'application/json'.

APPEND ls_header TO lt_headerparam.

requestpost = cl_ble_http_request=>create(
)->set_method(
    'POST'
)->set_header_parameters(
    parameters =  lt_headerparam
)->set_body(
    data =  lv_body_post ).

*Send http request
TRY.
    DATA(responsePOST) = lo_client->send( requestpost ).
    DATA(lv_bodyResponse) = responsePost->get_body( ).
  CATCH cx_ble_http_exception INTO DATA(lx_Response).
    customerrecord-dqmresponse = lx_Response->get_text( ).
    EXIT.
ENDTRY.
**************Parsing the response*******************************
DATA : addr_info_code   TYPE string,
       country          TYPE string,
       postcode         TYPE string,
       shipping_address TYPE string,
       city             TYPE string,
       lv_string1       TYPE string,
       lv_string2       TYPE string,
       lv_pair          TYPE string,
       suggest_count    TYPE string,
       suggestions      TYPE string,
       lv_key           TYPE string,
       lv_value         TYPE string.

*There are newline characters in the response of DQS, that need to be removed
REPLACE ALL OCCURRENCES OF SUBSTRING cl_abap_char_utilities=>newline IN lv_bodyresponse WITH ''.

SPLIT lv_bodyresponse AT '],' INTO lv_string1 lv_string2.
SPLIT lv_string1 AT '[' INTO lv_string1 suggestions.

REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_string1 WITH ''.
REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_string2 WITH ''.

**** Loop over lv_string1 to extract all key values pairs from json ****
SPLIT lv_string1 AT ',' INTO lv_pair lv_string1.
WHILE NOT ( lv_string1 EQ lv_pair ).
  SPLIT lv_pair AT ':' INTO lv_key lv_value.
  REPLACE ALL OCCURRENCES OF SUBSTRING '{' IN lv_value WITH ''.
  REPLACE ALL OCCURRENCES OF SUBSTRING '}' IN lv_value WITH ''.
  CONDENSE lv_value.
  IF lv_key CS dqs_addrinfocode.
    addr_info_code = lv_value.
    customerrecord-dqmresponse = addr_info_code.
  ELSE.
    IF lv_key CS dqs_postcode.
      postcode = lv_value.
    ELSE.
      IF lv_key CS dqs_shippingaddress.
        shipping_address = lv_value.
      ELSE.
        IF lv_key CS dqs_country.
          country = lv_value.
        ELSE.
          IF lv_key CS dqs_city.
            city = lv_value.
          ELSE.
            IF lv_key CS dqs_suggestioncount.
              suggest_count = lv_value.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  SPLIT lv_string1 AT ',' INTO lv_pair lv_string1.
ENDWHILE.
****End of loop over lv_string1 *****

**** Loop over lv_string2 to extract all key values pairs from json ****
SPLIT lv_string2 AT ',' INTO lv_pair lv_string2.
WHILE NOT ( lv_string2 EQ lv_pair ).
SPLIT lv_pair AT ':' INTO lv_key lv_value.
  REPLACE ALL OCCURRENCES OF SUBSTRING '{' IN lv_value WITH ''.
  REPLACE ALL OCCURRENCES OF SUBSTRING '}' IN lv_value WITH ''.
  CONDENSE lv_value.
  IF lv_key CS dqs_addrinfocode.
    addr_info_code = lv_value.
    customerrecord-dqmresponse = addr_info_code.
  ELSE.
    IF lv_key CS dqs_postcode.
      postcode = lv_value.
    ELSE.
      IF lv_key CS dqs_shippingaddress.
        shipping_address = lv_value.
      ELSE.
        IF lv_key CS dqs_country.
          country = lv_value.
        ELSE.
          IF lv_key CS dqs_city.
            city = lv_value.
          ELSE.
            IF lv_key CS dqs_suggestioncount.
              suggest_count = lv_value.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  SPLIT lv_string2 AT ',' INTO lv_pair lv_string2.
ENDWHILE.
**** End of loop over the lv_string2 ****

*** Translate errorcode into error message
CASE customerrecord-dqmresponse.
    WHEN '1030'.
        customerrecord-dqmresponse = 'No country identified.'.
    WHEN '1080'.
        customerrecord-dqmresponse = 'Address contains at least one character that is not part of the supported character set. '.
    WHEN '2000'.
        customerrecord-dqmresponse = 'Unable to identify city, region, and/or postcode information.'.
    WHEN '2020'.
        customerrecord-dqmresponse = 'Unable to identify postcode, and invalid city is preventing address cleansing.'.
    WHEN '2030'.
        customerrecord-dqmresponse = 'Invalid city and postcode are preventing address cleansing.'.
    WHEN '2040'.
        customerrecord-dqmresponse = 'Invalid postcode is preventing a city selection.'.
    WHEN '2050'.
        customerrecord-dqmresponse = 'City, region, and postcode matches are too close to choose one. '.
    WHEN '2070'.
        customerrecord-dqmresponse = 'City is valid, but multiple possible postcodes prevent choosing one.'.
    WHEN '3000'.
        customerrecord-dqmresponse = 'City, region, and postcode are valid, but unable to identify the street address. '.
    WHEN '3010'.
        customerrecord-dqmresponse = 'City, region, and postcode are valid, but unable to match street name to directory. '.
    WHEN '3020'.
        customerrecord-dqmresponse = 'Possible street name matches are too close to choose one.'.
    WHEN '3030'.
        customerrecord-dqmresponse = 'House number is missing on input or not in the directory.'.
    WHEN '3050'.
        customerrecord-dqmresponse = 'An invalid or missing street type is preventing address cleansing.'.
    WHEN '3060'.
        customerrecord-dqmresponse = 'A missing street type and prefix/suffix is preventing address cleansing.'.
    WHEN '3070'.
        customerrecord-dqmresponse = 'An invalid or missing prefix/suffix is preventing address cleansing.'.
    WHEN '3080'.
        customerrecord-dqmresponse = 'An invalid or missing postcode is preventing address cleansing.'.
    WHEN '3090'.
        customerrecord-dqmresponse = 'An invalid or missing city is preventing address cleansing.'.
    WHEN '3100'.
        customerrecord-dqmresponse = 'Possible address matches are too close to choose one.'.
    WHEN '3200'.
        customerrecord-dqmresponse = 'The building is missing on input or not in the directory.'.
    WHEN '3210'.
        customerrecord-dqmresponse = 'The building s address is not in the directory.'.
    WHEN '3220'.
        customerrecord-dqmresponse = 'Possible building matches are too close to choose one.'.
    WHEN '3250'.
        customerrecord-dqmresponse = 'The house number or building is missing on input or both are not in the directory. '.
    WHEN '3300'.
        customerrecord-dqmresponse = 'The postcode-only lookup returned multiple street names.'.
    WHEN '4000'.
        customerrecord-dqmresponse = 'The secondary address information is missing on input or not in the directory. '.
    WHEN '4010'.
        customerrecord-dqmresponse = 'Possible secondary address matches are too close to choose one.'.
    WHEN '4500'.
        customerrecord-dqmresponse = 'The organization is missing on input or not in the directory.'.
    WHEN '4510'.
        customerrecord-dqmresponse = 'The organization s address is not in the directory.'.
    WHEN '4520'.
        customerrecord-dqmresponse = 'Possible organization matches are too close to choose one.'.
    WHEN '5000'.
        customerrecord-dqmresponse = 'The postal authority classifies this address as undeliverable.'.
    WHEN '5020'.
        customerrecord-dqmresponse = 'The input address is blank.'.
    WHEN '5040'.
        customerrecord-dqmresponse = 'A violation of city, region, and postcode assignment rules is preventing address cleansing.'.
    WHEN '5050'.
        customerrecord-dqmresponse = 'The address is an obsolete address and can be matched to multiple addresses.'.
    WHEN '5500'.
        customerrecord-dqmresponse = 'Assignment to the reference data is below the specified minimum assignment level.'.
ENDCASE.

**** Variables for parsing the suggestions ****
DATA: sugg_addr_address_delivery    TYPE string,
      sugg_addr_selection           TYPE string,
      sugg_addr_single_address      TYPE string,
      sugg_addr_range_type          TYPE string,
      sugg_addr_lastline            TYPE string,
      sugg_addr_sec_side_indicator  TYPE string,
      sugg_addr_prim_side_indicator TYPE string,
      one_suggestion                TYPE string,
      suggestion                    TYPE yy1_suggestion_faultyaddress,
      write_api_suggestion          TYPE REF TO IF_CBO_WRITE_NODE,
      lt_suggestion_api             TYPE TABLE OF REF TO IF_CBO_WRITE_NODE.

IF ( suggest_count = 'null' ) OR ( suggest_count = 0 ).
    IF faultyaddresscount GT 0.
    	lt_suggestion_api = write_api_faultyaddress->GET_CHILDREN(
		    node_id = 'Suggestion'
	    ).
	    LOOP AT lt_suggestion_api INTO write_api_suggestion.
		    write_api_suggestion->delete( ).
	    ENDLOOP.
    ENDIF.
  	IF addr_info_code EQ ''.
**** write Data comming from DQS into fields and set quality Level to High ****
		customerrecord-postcode = postcode.
		customerrecord-shippingaddress = shipping_address.
		customerrecord-country = country.
		customerrecord-city = city.
		customerrecord-qualitylevel = 'HI'.

**** Update Faultyaddress ****
        IF faultyaddresscount GT 0.
    	  ls_faultyaddress-addressid = lv_faultyaddress_key.
    	  ls_faultyaddress-customerrecord = customerrecord-orderid.
    	  ls_faultyaddress-shippingaddress = customerrecord-shippingaddress.
    	  ls_faultyaddress-country = customerrecord-country.
    	  ls_faultyaddress-postcode = customerrecord-postcode.
    	  ls_faultyaddress-city = customerrecord-city.
    	  ls_faultyaddress-dateofvalidation = tsl.
    	  ls_faultyaddress-qualitylevel = customerrecord-qualitylevel.
    	  ls_faultyaddress-dqmresponse = customerrecord-dqmresponse.

    	  write_api_faultyaddress->update(
    		CHANGING data = ls_faultyaddress
    	  ).
        ENDIF.
  	ELSE.
        customerrecord-qualitylevel = 'LO'.
*   fill faultyaddress with data
        ls_faultyaddress-addressid = lv_faultyaddress_key.
	    ls_faultyaddress-customerrecord = customerrecord-orderid.
	    ls_faultyaddress-shippingaddress = customerrecord-shippingaddress.
	    ls_faultyaddress-country = customerrecord-country.
	    ls_faultyaddress-postcode = customerrecord-postcode.
	    ls_faultyaddress-city = customerrecord-city.
	    ls_faultyaddress-dateofvalidation = tsl.
	    ls_faultyaddress-qualitylevel = customerrecord-qualitylevel.
	    ls_faultyaddress-dqmresponse = customerrecord-dqmresponse.

	    IF faultyaddresscount GT 0.
**** Update Faulty Address and delete existing suggestions ****
    	    write_api_faultyaddress->update(
    		    CHANGING data = ls_faultyaddress
    	    ).
	    ELSE.
**** Create new Faultyaddress ****
            write_api_faultyaddress = write->create_root(
                EXPORTING
                  business_object_id   = 'YY1_FAULTYADDRESS'
                  data                 = ls_faultyaddress
            ).
	    ENDIF.
    ENDIF.
ELSE.
    customerrecord-qualitylevel = 'LO'.

	ls_faultyaddress-addressid = lv_faultyaddress_key.
	ls_faultyaddress-customerrecord = customerrecord-orderid.
    ls_faultyaddress-shippingaddress = customerrecord-shippingaddress.
    ls_faultyaddress-country = customerrecord-country.
    ls_faultyaddress-postcode = customerrecord-postcode.
    ls_faultyaddress-city = customerrecord-city.
    ls_faultyaddress-dateofvalidation = tsl.
    ls_faultyaddress-qualitylevel = customerrecord-qualitylevel.
    ls_faultyaddress-dqmresponse = customerrecord-dqmresponse.

    IF faultyaddresscount GT 0.
**** Update Faulty Address and delete existing suggestions ****
	    write_api_faultyaddress->update(
		    CHANGING data = ls_faultyaddress
	    ).

	    lt_suggestion_api = write_api_faultyaddress->get_children(
		    node_id = 'Suggestion'
	    ).
	    LOOP AT lt_suggestion_api INTO write_api_suggestion.
		    write_api_suggestion->delete( ).
	    ENDLOOP.
	ELSE.
**** Create new Faultyaddress ****
        write_api_faultyaddress = write->create_root(
            EXPORTING
              business_object_id = 'YY1_FAULTYADDRESS'
              data                 = ls_faultyaddress
        ).
	ENDIF.

*****************Parse the suggestions ******************************
    IF suggest_count = 1.
        REPLACE ALL OCCURRENCES OF SUBSTRING '{' IN suggestions WITH ''.
        REPLACE ALL OCCURRENCES OF SUBSTRING '}' IN suggestions WITH ''.
        SPLIT suggestions AT ':' INTO lv_key suggestions.

        WHILE NOT ( suggestions EQ lv_key ).
          IF lv_key CS dqs_sugg_selection.
            SPLIT suggestions AT ',' INTO lv_value suggestions.
            REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_value WITH ''.
            CONDENSE lv_value.
            sugg_addr_selection = lv_value.
          ELSE.
            SPLIT suggestions AT '",' INTO lv_value suggestions.
            REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_value WITH ''.
            CONDENSE lv_value.
            IF lv_key CS dqs_sugg_addrdeli.
              sugg_addr_address_delivery = lv_value.
            ELSE.
              IF lv_key CS dqs_sugg_singleaddr.
                sugg_addr_single_address = lv_value.
              ELSE.
                IF lv_key CS dqs_sugg_rangetype.
                  sugg_addr_range_type = lv_value.
                ELSE.
                  IF lv_key CS dqs_sugg_lastline.
                    sugg_addr_lastline = lv_value.
                  ELSE.
                    IF lv_key CS dqs_sugg_secindica.
                      sugg_addr_sec_side_indicator = lv_value.
                    ELSE.
                      IF lv_key CS dqs_sugg_primindica.
                        sugg_addr_prim_side_indicator = lv_value.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        SPLIT suggestions AT ':' INTO lv_key suggestions.
        ENDWHILE.
********************Write suggestion into FaultyAddress BO************************
        suggestion = VALUE #(
            addressdelivery = sugg_addr_address_delivery
            selection = sugg_addr_selection
            singleaddress =  sugg_addr_single_address
            rangetype = sugg_addr_range_type
            lastline = sugg_addr_lastline
            secsideindicator = sugg_addr_sec_side_indicator
            primsideindicator = sugg_addr_prim_side_indicator
        ).
        write_api_faultyaddress->create_child(
            EXPORTING
                node_id = 'Suggestion'
                data    = suggestion
        ).
    ELSE.
        SPLIT suggestions AT '},' INTO one_suggestion suggestions.
        WHILE NOT ( suggestions EQ one_suggestion ).
          SPLIT one_suggestion AT ':' INTO lv_key one_suggestion.
          WHILE NOT ( one_suggestion EQ lv_key ).
            REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_value WITH ''.
            IF lv_key CS dqs_sugg_selection.
              SPLIT one_suggestion AT ',' INTO lv_value one_suggestion.
              REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_value WITH ''.
              CONDENSE lv_value.
              sugg_addr_selection = lv_value.
            ELSE.
              SPLIT one_suggestion AT '",' INTO lv_value one_suggestion.
              REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_value WITH ''.
              REPLACE ALL OCCURRENCES OF SUBSTRING '}' IN lv_value WITH ''.
              CONDENSE lv_value.
              IF lv_key CS dqs_sugg_addrdeli.
                sugg_addr_address_delivery = lv_value.
              ELSE.
                IF lv_key CS dqs_sugg_singleaddr.
                  sugg_addr_single_address = lv_value.
                ELSE.
                  IF lv_key CS dqs_sugg_rangetype.
                    sugg_addr_range_type = lv_value.
                  ELSE.
                      IF lv_key CS dqs_sugg_lastline.
                      	sugg_addr_lastline = lv_value.
                      ELSE.
                        IF lv_key CS dqs_sugg_secindica.
                          sugg_addr_sec_side_indicator = lv_value.
                      	ELSE.
                          IF lv_key CS dqs_sugg_primindica.
                          	sugg_addr_prim_side_indicator = lv_value.
                          ENDIF.
                      	ENDIF.
                      ENDIF.
                  	ENDIF.
                  ENDIF.
              	ENDIF.
              ENDIF.
            SPLIT one_suggestion AT ':' INTO lv_key one_suggestion.
          ENDWHILE.
************Write one suggestion into faulty address BO ******************
            suggestion = VALUE #(
              addressdelivery = sugg_addr_address_delivery
              selection = sugg_addr_selection
              singleaddress =  sugg_addr_single_address
              rangetype = sugg_addr_range_type
              lastline = sugg_addr_lastline
              secsideindicator = sugg_addr_sec_side_indicator
              primsideindicator = sugg_addr_prim_side_indicator
            ).
            write_api_faultyaddress->create_child(
                EXPORTING
                  node_id = 'Suggestion'
                  data    = suggestion
            ).
            SPLIT suggestions AT '},' INTO one_suggestion suggestions.
        ENDWHILE.
    ENDIF.
ENDIF.
