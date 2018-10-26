@echo off
REM
REM author Everton de Vargas Agilar <<evertonagilar@gmail.com>>
REm
erl -pa .\ebin deps\rfc3339\ebin deps\jiffy\ebin deps\jesse\ebin deps\jsx\ebin deps\poolboy\ebin deps\cowboy\ebin deps\cowlib\ebin deps\json_rec\ebin deps\mochiweb\ebin deps\oauth2\ebin deps\parse_trans\ebin deps\ranch\ebin deps\esqlite\ebin deps\soap\ebin -sname emsbus -setcookie erlangms -eval "ems_bus:start()" -boot start_sasl -config .\priv\conf\elog



		
