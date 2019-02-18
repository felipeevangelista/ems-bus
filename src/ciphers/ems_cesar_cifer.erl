-module(ems_cesar_cifer).

-export([encrypt/2, decrypt/2, execute/1]).


execute(Request) ->  
    io:format("Aqui 1 >>>>>>>>>>>>>> ~n"), 
    CipherText = encrypt("teste para verificar criptografia",3),
    PlainText = decrypt(CipherText, 3),
    io:format("CipherText: ~p~n~n",[CipherText]),
    io:format("PlainText: ~p~n~n",[PlainText]),
    {ok, Request}.

encrypt(PlainText, Padding) ->
    io:format("PlainText >>> ~p~n",[PlainText]),
    PlainTextList = binary_to_list(PlainText),
    io:format("PlainTextList: ~p~n~n",[PlainTextList]),
    CipherTextList = lists:map(fun(Item) -> Item + Padding end, PlainTextList),
    ems_util:list_to_binlist(CipherTextList).


decrypt(CipherText, Padding) ->
        CipherTextList = binary_to_list(CipherText),
        io:format("CipherTextList: ~p~n~n",[CipherTextList]),
        PlainTextList = lists:map(fun(Item) -> Item - Padding end, CipherTextList),
        ems_util:list_to_binlist(PlainTextList).