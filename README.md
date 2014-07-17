CoAP (partial) library for Erlang
==========================

This library implements (as of today) only one of the [CoAP methods](http://tools.ietf.org/html/draft-ietf-core-coap-18), the GET method, using [CantCoAP](https://github.com/staropram/cantcoap) as a serialization library for the C driver.\<h3\><br />

Example
--------------------------
### Get method
		coap_client:get("192.168.1.100","temp",[{urihost,"2001::1"},{uriport,"5678"}]).\<h3\><br />
### Put method
		coap_client:put("192.168.1.100","leds",[{urihost,"2001::1"},{uriport,"5678"},{uriquery,"7"}]).\<h3\><br />
### Delete method
		coap_client:delete("192.168.1.100","leds",[{urihost,"2001::1"},{uriport,"5678"},{uriquery,"5"}]).\<h3\><br />
