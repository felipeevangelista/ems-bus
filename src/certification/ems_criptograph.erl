-module(ems_criptograph).

-include("../include/ems_schema.hrl").

-export([read_private_key/1, read_public_key/1, read_certificate/1 ,read_pdf/1, execute/1]).



execute(Request) -> 
    %%PrivateKey = read_private_key("/home/rcarauta/desenvolvimento/certificado/client.der"),
    Certificate = read_certificate("/home/rcarauta/desenvolvimento/certificado/client.crt"),
    %%PDF = read_pdf("/home/rcarauta/Downloads/file.pdf"),

    io:format("Data2: ~p~n",[Certificate]),

    ListFilesAuthorities = read_all_files_path("/home/rcarauta/desenvolvimento/certificado/autoridades"),

    io:format("Valid certificates: ~p~n~n",[verify_valid_certificate(Certificate, ListFilesAuthorities)]),

    {ok, Request#request{code = 200,
            content_type_out = <<"application/json">>,
            response_data = <<"{\"response:\" \" Work correctlly!\"}">>}
	}.

read_private_key(FilePrivateKey) ->
 	 ems_util:open_file(FilePrivateKey).
	

read_public_key(FilePublicKey) ->
     ems_util:open_file(FilePublicKey).

read_certificate(FileCertificate) ->
    ContentCert = ems_util:open_file(FileCertificate),
    [ Certificate ] = public_key:pem_decode(ContentCert),
    Cert =  public_key:pem_entry_decode(Certificate),
    Data1 = element(2, Cert),
    PublicCertificateKey =  element(8, Data1),
    {ok, PublicCertificateKey}.


read_pdf(FilePDF) ->
    ems_util:open_file(FilePDF).
 

read_all_files_path(Dir) ->
    read_all_files_path(Dir, true).

read_all_files_path(Dir, FilesOnly) ->
    case filelib:is_file(Dir) of
        true ->
            case filelib:is_dir(Dir) of
                true -> {ok, read_all_files_path([Dir], FilesOnly, [])};
                false -> {error, enotdir}
            end;
        false -> {error, enoent}
    end.


read_all_files_path([], _FilesOnly, Acc) ->
    Acc;
read_all_files_path([Path|Paths], FilesOnly, Acc) ->
        read_all_files_path(Paths, FilesOnly,
        case filelib:is_dir(Path) of
            false -> [Path | Acc];
            true ->
                {ok, Listing} = file:list_dir(Path),
                SubPaths = [filename:join(Path, Name) || Name <- Listing],
                read_all_files_path(SubPaths, FilesOnly,
                    case FilesOnly of
                        true -> Acc;
                        false -> [Path | Acc]
                    end)
        end).


verify_valid_certificate(Certificate, ListFilesAutorities) ->
    Result = case ListFilesAutorities of
        {ok, ListFiles} -> 
             ListFiles;
        _ -> {error, invalid_format}
    end,
    iterator_list(Certificate, Result).


iterator_list(_Certificate,[]) -> 
    false;
iterator_list(Certificate,[H|T]) ->
    ContentCertificateAuthority = read_certificate(H),
    case Certificate of
        ContentCertificateAuthority -> true;
        _ ->  iterator_list(Certificate,T)
    end.

    
