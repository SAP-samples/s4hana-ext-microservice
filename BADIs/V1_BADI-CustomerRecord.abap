* After Modify Determination for Node ID CUSTOMERRECORD
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
        dqs_configname      TYPE string VALUE 'AddressValidation'.

*Get unchanged data from Database
SELECT *
	FROM YY1_CUSTOMERRECORD
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

************** SET CONNECTION TO DQS *********************
DATA:   lt_headerparam  TYPE TIHTTPNVP,
        ls_header       TYPE LINE OF TIHTTPNVP,
        lv_body_post    TYPE string,
        lv_available    TYPE abap_bool.

*Check if connection is available
cl_ble_http_client=>is_service_available(
  EXPORTING
    communication_scenario = 'YY1_CUSTOMERRECORD'
    outbound_service       = 'YY1_DQS_REST'
  RECEIVING
 available              = lv_available
).

IF NOT !lv_available = abap_true.
    customerrecord-dqmresponse = 'Service seems to be unavailable, please check the communication scenario.'.
    EXIT.
ENDIF.

*create http client
DATA(lo_client) = cl_ble_http_client=>create(
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

APPEND ls_header to lt_headerparam.

DATA(requestPOST) = cl_ble_http_request=>create(
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
       lv_rest          TYPE string,
       lv_pair          TYPE string,
       lv_key           TYPE string,
       lv_value         TYPE string.

REPLACE ALL OCCURRENCES OF SUBSTRING cl_abap_char_utilities=>newline IN lv_bodyresponse WITH ''.
REPLACE ALL OCCURRENCES OF SUBSTRING '"' IN lv_bodyresponse WITH ''.

SPLIT lv_bodyresponse AT ',' INTO lv_pair lv_bodyresponse.
WHILE NOT ( lv_bodyresponse EQ lv_pair ).
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
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  SPLIT lv_bodyresponse AT ',' INTO lv_pair lv_bodyresponse.
ENDWHILE.

*Set address quality level
IF addr_info_code EQ ''.
*** write Data comming from DQS into fields and set quality Level to High ****
	customerrecord-postcode = postcode.
	customerrecord-shippingaddress = shipping_address.
	customerrecord-country = country.
	customerrecord-city = city.
	customerrecord-qualitylevel = 'HI'.
ELSE.
*** Set quality to low and translate errorcode into error message
	customerrecord-qualitylevel = 'LO'.
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
ENDIF.
