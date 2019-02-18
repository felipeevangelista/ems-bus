-module(ems_criptograph).

-include("../include/ems_schema.hrl").

-export([read_private_key/1, read_public_key/1, read_certificate/1 ,read_pdf/1, execute/1, verify_sign/3]).



execute(Request) -> 
    PrivateKey = read_private_key("/media/rcarauta/SSD/desenvolvimento/barramento/ems-bus/src/certification/private.pem"),
    Certificate = read_certificate("/home/rcarauta/desenvolvimento/certificado/client.crt"),
    PDF = read_pdf("/home/rcarauta/Downloads/file.pdf"),

    ListFilesAuthorities = read_all_files_path("/home/rcarauta/desenvolvimento/certificado/autoridades"),

    io:format("Valid certificates: ~p~n~n",[verify_valid_certificate(Certificate, ListFilesAuthorities)]),

    io:format("Data2: ~p~n",[Certificate]),

    sign_pdf(PrivateKey, PDF),

    PublicKey = read_public_key("/media/rcarauta/SSD/desenvolvimento/barramento/ems-bus/src/certification/public.pem"),

    Verified = verify_sign(PDF, "/media/rcarauta/SSD/desenvolvimento/barramento/ems-bus/src/certification/fileSign", PublicKey),

    io:format("Verified >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ~p~n~n",[Verified]),

    io:format("Funcionou perfeitamente bem >>>>>>>>>>>>>>>>>>>>>>>>>>!!!!!!!!!!!!!!!!!!!! :) ~n~n~n~n"),

    {ok, Request#request{code = 200,
            content_type_out = <<"application/json">>,
            response_data = <<"{\"response:\" \" Work correctlly!\"}">>}
	}.

read_private_key(FilePrivateKey) ->
      RawSKey =  ems_util:open_file(FilePrivateKey),
      [EncSKey] = public_key:pem_decode(RawSKey),
      SKey = public_key:pem_entry_decode(EncSKey),
      SKey.
	

read_public_key(FilePublicKey) ->
        RawPKey =  ems_util:open_file(FilePublicKey),
        [EncPKey] = public_key:pem_decode(RawPKey),
        PKey = public_key:pem_entry_decode(EncPKey),
        PKey.

read_certificate(FileCertificate) ->
    ContentCert = ems_util:open_file(FileCertificate),
    [ Certificate ] = public_key:pem_decode(ContentCert),
    Cert =  public_key:pem_entry_decode(Certificate),
    Data1 = element(2, Cert),
    PublicCertificateKey =  element(8, Data1),
    {ok, PublicCertificateKey}.


read_pdf(FilePDF) ->
    ems_util:open_file(FilePDF).
 

sign_pdf(PrivKey, Msg) ->
    DigestType = sha256,
    SigBin = public_key:sign(Msg, DigestType, PrivKey),
    io:format("Assinado >>>>>>>>>>>>>>>>>>>> ~n"),
    file:write_file("/media/rcarauta/SSD/desenvolvimento/barramento/ems-bus/src/certification/fileSign", SigBin).


verify_sign(Msg, SignatureFile ,PublicKey) ->
    DigestType = sha256,
    Signature = ems_util:open_file(SignatureFile),
    public_key:verify(Msg, DigestType, Signature, PublicKey).


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

    
