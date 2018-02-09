%%********************************************************************
%% @title Module ems_http_server
%% @version 1.0.0
%% @doc Main module HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_http_handler).

-include("include/ems_config.hrl").
-include("include/ems_schema.hrl").

-export([init/2, terminate/3]).

-record(state, {http_max_content_length,
				http_header_default,
				http_header_options}).


init(CowboyReq, State = #state{http_header_default = HttpHeaderDefault,
							   http_header_options = HttpHeaderOptions}) ->
	case ems_util:encode_request_cowboy(CowboyReq, self(), HttpHeaderDefault, HttpHeaderOptions) of
		{ok, Request = #request{t1 = T1}, Service, CowboyReq2} -> 
			case ems_dispatcher:dispatch_request(Request, Service) of
				{ok, request, Request2 = #request{code = Code,
												  response_header = ResponseHeader,
												  response_data = ResponseData,
												  content_type_out = ContentTypeOut}} ->
					case ContentTypeOut of
						<<"application/x-www-form-urlencoded; charset=UTF-8">> ->
							ems_db:inc_counter(http_content_type_out_form_urlencode);
						<<"application/x-www-form-urlencoded">> ->
							ems_db:inc_counter(http_content_type_out_form_urlencode);
						<<"application/json">> ->
							ems_db:inc_counter(http_content_type_out_application_json);
						<<"application/json; charset=utf-8">> ->
							ems_db:inc_counter(http_content_type_out_application_json);
						<<"application/json;charset=utf-8">> -> 
							ems_db:inc_counter(http_content_type_out_application_json);
						<<"application/xml">> ->
							ems_db:inc_counter(http_content_type_out_application_xml);
						<<"application/pdf">> ->
							ems_db:inc_counter(http_content_type_out_application_pdf);
						<<"text/html">> ->
							ems_db:inc_counter(http_content_type_out_text_html);
						<<"application/xhtml+xml">> ->
							ems_db:inc_counter(http_content_type_out_application_xhtml_xml);
						<<"text/css">> ->
							ems_db:inc_counter(http_content_type_out_text_css);
						<<"application/x-javascript">> ->
							ems_db:inc_counter(http_content_type_out_javascript);
						<<"image/png">> ->
							ems_db:inc_counter(http_content_type_out_image_png);
						<<"image/x-icon">> ->
							ems_db:inc_counter(http_content_type_out_image_xicon);
						<<"image/gif">> ->
							ems_db:inc_counter(http_content_type_out_image_gif);
						<<"image/jpeg">> ->
							ems_db:inc_counter(http_content_type_out_image_jpeg);
						<<"application/font-woff">> ->
							ems_db:inc_counter(http_content_type_out_font_woff);
						<<"image/bmp">> ->
							ems_db:inc_counter(http_content_type_out_image_bpm);
						<<"text/csv">> ->
							ems_db:inc_counter(http_content_type_out_text_csv);
						_ ->
							ems_db:inc_counter(http_content_type_out_other)
					end,
					Response = cowboy_req:reply(Code, 
												ResponseHeader#{
													<<"content-type">> => ContentTypeOut,
													<<"access-control-allow-origin">> => ?ACCESS_CONTROL_ALLOW_ORIGIN,
													<<"access-control-max-age">> => ?ACCESS_CONTROL_MAX_AGE,
													<<"access-control-allow-headers">> => ?ACCESS_CONTROL_ALLOW_HEADERS,
													<<"access-control-allow-methods">> => ?ACCESS_CONTROL_ALLOW_METHODS,
													<<"access-control-expose-headers">> => ?ACCESS_CONTROL_EXPOSE_HEADERS}, 
												ResponseData, 
												CowboyReq2),
					ems_logger:log_request(Request2);
				{error, request, Request2 = #request{code = Code,
													 response_header = ResponseHeader,
													 response_data = ResponseData}} ->
					Response = cowboy_req:reply(Code, 
												ResponseHeader,
												ResponseData, 
												CowboyReq2),
					ems_logger:log_request(Request2);
				{error, Reason} = Error ->
					Request2 = Request#request{code = 400, 
											    content_type_out = ?CONTENT_TYPE_JSON,
											    reason = Reason, 
											    response_data = ems_schema:to_json(Error), 
											    response_header = Request#request.response_header,
											    latency = ems_util:get_milliseconds() - T1},
					Response = cowboy_req:reply(Request2#request.code, 
												Request2#request.response_header, 
												Request2#request.response_data, CowboyReq2),
					ems_logger:log_request(Request2)
			end;
		{_, request, #request{code = Code,
									  response_header = ResponseHeader,
									  response_data = ResponseData}, CowboyReq2} ->
					Response = cowboy_req:reply(Code, 
												ResponseHeader, 
												ResponseData, 
												CowboyReq2);
		{error, Reason} -> 
			Url = binary_to_list(cowboy_req:path(CowboyReq)),
			{Ip, _} = cowboy_req:peer(CowboyReq),
			Ip2 = inet_parse:ntoa(Ip),
			ems_logger:error("ems_http_handler ~s from ~s: ~p.\n\t~p", [Url, Ip2, Reason, CowboyReq]),
			Response = cowboy_req:reply(400, HttpHeaderDefault, ?EINVALID_HTTP_REQUEST, CowboyReq)
	end,
	{ok, Response, State}.


terminate(_Reason, _Req, _State) ->  ok.    
