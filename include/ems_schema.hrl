%%********************************************************************
%% @title ems_schema
%% @version 1.0.0
%% @doc It contains definitions of the data structures used.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-record(sequence, {key :: atom(), 
				   index :: non_neg_integer()}).

-record(counter, {key :: atom(), 
     			  value :: non_neg_integer()}).

-record(user, {id :: non_neg_integer(), 					%%  1 - id       				-> TB_Usuario.UsuId
			   codigo :: non_neg_integer(),					%%  2 - codigo   				-> Tb_Pessoa.PesCodigoPessoa
			   login :: binary(),							%%  3 - login	
			   name :: binary(), 							%%  4 - name		
			   cpf :: binary(),								%%  5 - cpf		
			   email :: binary(), 							%%  6 - email	
			   password :: binary(),						%%  7 - password 
			   dt_expire_password :: binary(),				%%  8 - dt_expire_password
			   type :: non_neg_integer(),					%%  9 - type       				-> 0 = interno  1 = tecnico  2 = docente  3 = discente, 4 = terceiros
			   subtype :: non_neg_integer(),				%% 10 - subtype 				-> se aluno códigos abaixo:
															%%			  		 			      1 = extensao 2 = graduacao 3 = aperfeicoamento 4 = especializacao 5 = mestrado 
															%%   					              6 = doutorado 7 = pos-doutorado 8 = residencia 9 = aluno especial - graduacao 
															%%           					     10 = aluno especial - pos-graduacao 11 = estagio em pos-graduacao
			   passwd_crypto :: binary(),					%% 11 - passwd_crypto 			-> Algoritmo criptografia: SHA1
			   type_email :: non_neg_integer(),				%% 12 - type_email				-> undefined = desconhecido  1 = Institucional  2 = Pessoal
			   active :: boolean(),							%% 13 - active
			   endereco :: binary(),						%% 14 - endereco
			   complemento_endereco :: binary(),			%% 15 - complemento_endereco
			   bairro :: binary(),							%% 16 - bairro
			   cidade :: binary(),							%% 17 - cidade
			   uf :: binary(),								%% 18 - uf
			   cep :: binary(),								%% 19 - cep
			   rg :: binary(),								%% 20 - rg	
			   data_nascimento :: binary(),					%% 21 - data_nascimento
			   sexo :: non_neg_integer(),					%% 22 - sexo
			   telefone :: binary(),						%% 23 - telefone
			   celular :: binary(),							%% 24 - celular
			   ddd :: binary(),								%% 25 - ddd
			   nome_pai :: binary(),						%% 26 - nome_pai
			   nome_mae :: binary(),						%% 27 - nome_ae
			   nacionalidade :: non_neg_integer(),			%% 28 - nacionalidade
			   remap_user_id :: non_neg_integer(),			%% 29 - remap_user_id
			   admin :: boolean(),							%% 30 - admin					-> alguns web services podem ser acedidos somente por admins
			   ctrl_path :: string(),						%% 31 - ctrl_path
			   ctrl_file :: string(),						%% 32 - ctrl_file
			   ctrl_insert :: binary(),						%% 33 - ctrl_insert				-> Data que foi inserido no banco mnesia
			   ctrl_update :: binary(), 					%% 34 - ctrl_update				-> Data que foi atualiado no banco mnesia			
			   ctrl_modified :: binary(),					%% 35 - ctrl_modified			-> Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash :: non_neg_integer()				%% 36 - ctrl_hash 				-> Hash gerado para poder comparar dois registros	
		}).
		
-define(USER_SCHEMA_DESCRIPTOR, {
			   atom_type,									%%  0 - nome da tabela	
			   non_neg_integer_type, 						%%  1 - id   
			   non_neg_integer_type,						%%  2 - codigo
			   binary_type,									%%  3 - login	
			   binary_type, 								%%  4 - name		
			   binary_type,									%%  5 - cpf		
			   binary_type, 								%%  6 - email	
			   binary_type,									%%  7 - password 
			   binary_type,									%%  8 - dt_expire_password
			   non_neg_integer_type,						%%  9 - type
			   non_neg_integer_type,						%% 10 - subtype
			   binary_type,									%% 11 - passwd_crypto
			   non_neg_integer_type,						%% 12 - type_email
			   boolean_type,								%% 13 - active
			   binary_type,									%% 14 - endereco
			   binary_type,									%% 15 - complemento_endereco
			   binary_type,									%% 16 - bairro
			   binary_type,									%% 17 - cidade
			   binary_type,									%% 18 - uf
			   binary_type,									%% 19 - cep
			   binary_type,									%% 20 - rg	
			   binary_type,									%% 21 - data_nascimento
			   non_neg_integer_type,						%% 22 - sexo
			   binary_type,									%% 23 - telefone
			   binary_type,									%% 24 - celular
			   binary_type,									%% 25 - ddd
			   binary_type,									%% 26 - nome_pai
			   binary_type,									%% 27 - nome_ae
			   non_neg_integer_type,						%% 28 - nacionalidade
			   non_neg_integer_type,						%% 29 - remap_user_id
			   boolean_type,								%% 30 - admin	
			   string_type,									%% 31 - ctrl_path
			   string_type,									%% 32 - ctrl_file
			   binary_type,									%% 33 - ctrl_insert
			   binary_type, 								%% 34 - ctrl_update
			   binary_type,									%% 35 - ctrl_modified
			   non_neg_integer_type							%% 36 - ctrl_hash 	
			}).		
		
%
% Muitos atributos são armazenados no histórico pois as tabelas origem podem mudar
%
-record(user_history, {
			   id :: non_neg_integer(), 						%%  1 - id
			   user_id :: non_neg_integer(), 					%%  2 - user_id
			   user_codigo :: non_neg_integer(),				%%  3 - user_codigo
			   user_login :: binary(),							%%  4 - user_login
			   user_name :: binary(), 							%%  5 - user_name
			   user_cpf :: binary(),							%%  6 - user_cpf
			   user_email :: binary(), 							%%  7 - user_email
			   user_type :: non_neg_integer(),					%%  8 - user_type
			   user_subtype :: non_neg_integer(),				%%  9 - user_subtype
			   user_type_email :: non_neg_integer(),			%% 10 - user_type_email
			   user_active :: boolean(),						%% 11 - user_active
			   user_admin :: boolean(),							%% 12 - user_admin
			   client_id :: non_neg_integer(), 					%% 13 - client_id
			   client_name :: binary(), 						%% 14 - client_name
			   service_rowid :: non_neg_integer(), 				%% 15 - service_rowid
			   service_name :: binary(), 						%% 16 - service_name
			   service_url :: string(),  						%% 17 - service_url
			   service_type :: binary(),						%% 18 - service_type
			   service_service :: binary(),						%% 19 - service_service
			   service_use_re :: boolean(),						%% 20 - service_use_re
			   service_public :: boolean(), 					%% 21 - service_public
			   service_version :: binary(), 					%% 22 - service_version
			   service_owner :: binary(),  						%% 23 - service_owner
			   service_group :: binary(),  						%% 24 - service_group
			   owner :: binary(),  								%% 25 - owner
			   service_async :: boolean(),						%% 26 - service_async
			   request_rid :: non_neg_integer(),  				%% 27 - request_rid
			   request_type :: binary(),						%% 28 - request_type
			   request_uri :: binary(),							%% 29 - request_uri
			   request_url :: binary(),							%% 30 - request_url
			   request_url_masked :: boolean(),					%% 31 - request_url_masked
			   request_http_version :: binary(),				%% 32 - request_http_version
			   request_querystring :: binary(),					%% 33 - request_querystring
			   request_content_type_in :: binary(),				%% 34 - request_content_type_in
			   request_content_type_out :: binary(),			%% 35 - request_content_type_out
			   request_content_length :: non_neg_integer(), 	%% 36 - request_content_length
			   request_accept :: binary(),						%% 37 - request_accept
			   request_user_agent :: binary(),					%% 38 - request_user_agent
			   request_user_agent_version :: binary(),			%% 39 - request_user_agent_version
			   request_t1 :: non_neg_integer(),					%% 40 - request_t1
			   request_authorization :: binary(),				%% 41 - request_authorization
			   request_port :: non_neg_integer(),				%% 42 - request_port
			   request_bash :: non_neg_integer(),					%% 43 - request_bash
			   request_host :: binary(),						%% 44 - request_host
			   request_filename :: string(),					%% 45 - request_filename
			   request_referer :: binary(),						%% 46 - request_referer
			   request_access_token :: binary(),				%% 47 - request_access_token
			   request_operation :: atom(),						%% 48 - request_operation
			   request_reason_detail :: atom(),					%% 49 - request_reason_detail
			   request_reason :: atom(),						%% 50 - request_reason
			   request_code :: non_neg_integer(),	 			%% 51 - request_code
			   request_protocol :: atom(),						%% 52 - request_protocol
   			   request_timestamp :: binary()					%% 53 - request_timestamp
		}).


-define(USER_HISTORY_DESCRIPTOR, {
			   atom_type,										%%  0 - nome da tabela	
			   non_neg_integer_type, 							%%  1 - id   
			   non_neg_integer_type, 							%%  2 - user_id   
			   non_neg_integer_type,							%%  3 - user_codigo
			   binary_type,										%%  4 - user_login	
			   binary_type,										%%  5 - user_name	
			   binary_type,										%%  6 - user_cpf
			   binary_type,										%%  7 - user_email
			   non_neg_integer_type,							%%  8 - user_type
			   non_neg_integer_type,							%%  9 - user_subtype
			   non_neg_integer_type,							%% 10 - user_type_email
			   boolean_type,									%% 11 - user_active
			   boolean_type,									%% 12 - user_admin
			   non_neg_integer_type,							%% 13 - client_id
			   binary_type,										%% 14 - client_name
			   non_neg_integer_type,							%% 15 - service_rowid
			   binary_type,										%% 16 - service_name
			   string_type,										%% 17 - service_url
			   binary_type,										%% 18 - service_type
			   binary_type,										%% 19 - service_service
			   boolean_type,									%% 20 - service_use_re
			   boolean_type,									%% 21 - service_public
			   binary_type,										%% 22 - service_version
			   binary_type,										%% 23 - service_owner
			   binary_type,										%% 24 - service_group
			   binary_type,										%% 25 - owner
			   boolean_type,									%% 26 - service_async
			   non_neg_integer_type,							%% 27 - request_rid
			   binary_type,										%% 28 - request_type
			   binary_type,										%% 29 - request_uri
			   binary_type,										%% 30 - request_url
			   boolean_type,									%% 31 - request_url_masked
			   binary_type,										%% 32 - request_http_version
			   binary_type,										%% 33 - request_querystring
			   binary_type,										%% 34 - request_content_type_in
			   binary_type,										%% 35 - request_content_type_out
			   non_neg_integer_type,							%% 36 - request_content_length
			   binary_type,										%% 37 - request_accept
			   binary_type,										%% 38 - request_user_agent
			   binary_type,										%% 39 - request_user_agent_version
			   non_neg_integer_type,							%% 40 - request_t1
			   binary_type,										%% 41 - request_authorization
			   non_neg_integer_type,							%% 42 - request_port
			   non_neg_integer_type,							%% 43 - request_bash
			   binary_type,										%% 44 - request_host
			   string_type,										%% 45 - request_filename
			   binary_type,										%% 46 - request_referer
			   binary_type,										%% 47 - request_access_token
			   atom_type,										%% 48 - request_operation
			   atom_type,										%% 49 - request_reason_detail
			   atom_type,										%% 50 - request_reason
			   non_neg_integer,									%% 51 - request_code
			   atom_type,										%% 52 - request_protocol
			   binary_type										%% 53 - request_timestamp
			}).		

		
-record(user_dados_funcionais, {
			   id :: non_neg_integer(), 					%%  1 - id
			   type :: non_neg_integer(),					%%  2 - type     interno  1 = tecnico  2 = docente  3 = discente
			   subtype :: non_neg_integer(),				%%  3 - subtype  se aluno,  1 = extensao 2 = graduacao 3 = aperfeicoamento 4 = especializacao 5 = mestrado 6 = doutorado 7 = pos-doutorado 8 = residencia 9 = aluno especial - graduacao 10 = aluno especial - pos-graduacao 11 = estagio em pos-graduacao
			   active :: boolean(),							%%  4 - active
			   matricula :: non_neg_integer(),				%%  5 - matricula
			   ctrl_path :: string(),						%% 14 - ctrl_path
			   ctrl_file :: string(),						%% 15 - ctrl_file
			   ctrl_insert :: binary(),						%% 16 - ctrl_insert				-> Data que foi inserido no banco mnesia
			   ctrl_update :: binary(), 					%% 17 - ctrl_update				-> Data que foi atualiado no banco mnesia			
			   ctrl_modified :: binary(),					%% 18 - ctrl_modified			-> Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash :: non_neg_integer()				%% 19 - ctrl_hash 				-> Hash gerado para poder comparar dois registros	
		}).
		
-define(USER_DADOS_FUNCIONAIS_SCHEMA_DESCRIPTOR, {
			   atom_type,									%%  0 - nome da tabela	
			   non_neg_integer_type, 						%%  1 - id   
			   non_neg_integer_type,						%%  2 - type
			   non_neg_integer_type,						%%  3 - subtype
			   boolean_type,								%%  4 - active
			   non_neg_integer_type,						%%  5 - matricula
			   string_type,									%%  6 - ctrl_path
			   string_type,									%%  7 - ctrl_file
			   binary_type,									%%  8 - ctrl_insert
			   binary_type, 								%%  9 - ctrl_update
			   binary_type,									%% 10 - ctrl_modified
			   non_neg_integer_type							%% 11 - ctrl_hash 	
		}).
		

-record(user_email, {
			   id :: non_neg_integer(), 					%%  1 - id
			   codigo :: non_neg_integer(),					%%  2 - codigo
			   email :: binary(),							%%  3 - email
			   type :: non_neg_integer(),					%%  4 - type         1 = institucional  2 = outro
			   ctrl_path :: string(),						%%  5 - ctrl_path
			   ctrl_file :: string(),						%%  6 - ctrl_file
			   ctrl_insert :: binary(),						%%  7 - ctrl_insert				-> Data que foi inserido no banco mnesia
			   ctrl_update :: binary(), 					%%  8 - ctrl_update				-> Data que foi atualiado no banco mnesia			
			   ctrl_modified :: binary(),					%%  9 - ctrl_modified			-> Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash :: non_neg_integer()				%% 10 - ctrl_hash 				-> Hash gerado para poder comparar dois registros	
		}).

-define(USER_EMAIL_SCHEMA_DESCRIPTOR, {
			   atom_type,									%%  0 - nome da tabela	
			   non_neg_integer_type, 						%%  1 - id   
			   non_neg_integer_type,						%%  2 - codigo
			   binary_type,									%%  3 - email
			   non_neg_integer_type,						%%  4 - type
			   string_type,									%%  5 - ctrl_path
			   string_type,									%%  6 - ctrl_file
			   binary_type,									%%  7 - ctrl_insert
			   binary_type, 								%%  8 - ctrl_update
			   binary_type,									%%  9 - ctrl_modified
			   non_neg_integer_type							%% 10 - ctrl_hash 	
		}).

-record(user_endereco, {
			   id :: non_neg_integer(), 					%%  1 - id
			   codigo :: non_neg_integer(),					%%  2 - código
			   endereco :: binary(),						%%  3 - endereco
			   complemento :: binary(),						%%  4 - complemento
			   bairro :: binary(),							%%  5 - bairro
			   cidade :: binary(),							%%  6 - cidade
			   uf :: binary(),								%%  7 - uf
			   cep :: binary(),								%%  8 - cep
			   type :: non_neg_integer(),					%%  9 - type        1 = residencial  2 = comercial 3 = exterior 4 = outro
			   ctrl_path :: string(),						%% 10 - ctrl_path
			   ctrl_file :: string(),						%% 11 - ctrl_file
			   ctrl_insert :: binary(),						%% 12 - ctrl_insert				-> Data que foi inserido no banco mnesia
			   ctrl_update :: binary(), 					%% 13 - ctrl_update				-> Data que foi atualiado no banco mnesia			
			   ctrl_modified :: binary(),					%% 14 - ctrl_modified			-> Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash :: non_neg_integer()				%% 15 - ctrl_hash 				-> Hash gerado para poder comparar dois registros	
		}).

-define(USER_ENDERECO_SCHEMA_DESCRIPTOR, {
			   atom_type,									%%  0 - nome da tabela	
			   non_neg_integer_type, 						%%  1 - id   
			   non_neg_integer_type,						%%  2 - codigo
			   binary_type,									%%  3 - email
			   non_neg_integer_type,						%%  4 - type
			   string_type,									%%  5 - ctrl_path
			   string_type,									%%  6 - ctrl_file
			   binary_type,									%%  7 - ctrl_insert
			   binary_type, 								%%  8 - ctrl_update
			   binary_type,									%%  9 - ctrl_modified
			   non_neg_integer_type							%% 10 - ctrl_hash 	
		}).



-record(user_telefone, {
			   id :: non_neg_integer(), 					%%  1 - id
			   codigo :: non_neg_integer(),					%%  2 - codigo
			   numero :: binary(),							%%  3 - numero
			   ramal :: non_neg_integer(),					%%  4 - ramal
			   ddd :: binary(),								%%  5 - ddd
			   type :: non_neg_integer(),					%%  6 - type 		1 = celular  2 = comercial 3 = residencial
			   ctrl_path :: string(),						%%  7 - ctrl_path
			   ctrl_file :: string(),						%%  8 - ctrl_file
			   ctrl_insert :: binary(),						%%  9 - ctrl_insert				-> Data que foi inserido no banco mnesia
			   ctrl_update :: binary(), 					%% 10 - ctrl_update				-> Data que foi atualiado no banco mnesia			
			   ctrl_modified :: binary(),					%% 11 - ctrl_modified			-> Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash :: non_neg_integer()				%% 12 - ctrl_hash 				-> Hash gerado para poder comparar dois registros	
		}).

-define(USER_TELEFONE_SCHEMA_DESCRIPTOR, {
			   atom_type,									%%  0 - nome da tabela	
			   non_neg_integer_type, 						%%  1 - id   
			   non_neg_integer_type,						%%  2 - codigo
			   binary_type,									%%  3 - numero
			   non_neg_integer_type,						%%  4 - ramal
			   binary_type,									%%  5 - ddd
			   non_neg_integer_type,						%%  6 - type
			   string_type,									%%  7 - ctrl_path
			   string_type,									%%  8 - ctrl_file
			   binary_type,									%%  9 - ctrl_insert
			   binary_type, 								%% 10 - ctrl_update
			   binary_type,									%% 11 - ctrl_modified
			   non_neg_integer_type							%% 12 - ctrl_hash 	
		}).



-record(user_permission, {id :: non_neg_integer(),				%%  1 - id
						  user_id :: non_neg_integer(),			%%  2 - user_id
						  client_id :: non_neg_integer(),		%%  3 - client_id
						  perfil_id :: non_neg_integer(),		%%  4 - perfil_id
						  hash :: non_neg_integer(),			%%  5 - hash
						  hash2 :: non_neg_integer(),			%%  6 - hash2
						  name :: binary(),						%%  7 - name
						  url :: binary(),						%%  8 - url
						  grant_get :: boolean(),				%%  9 - grant_get
						  grant_post :: boolean(),				%% 10 - grant_post
						  grant_put :: boolean(),				%% 11 - grant_put
						  grant_delete :: boolean(),			%% 12 - grant_delete
						  position :: non_neg_integer(),		%% 13 - position
						  ctrl_path :: string(),				%% 14 - ctrl_path
						  ctrl_file :: string(),				%% 15 - ctrl_file
						  ctrl_insert :: binary(),				%% 16 - ctrl_insert				-> Data que foi inserido no banco mnesia
						  ctrl_update :: binary(), 				%% 17 - ctrl_update				-> Data que foi atualiado no banco mnesia			
						  ctrl_modified :: binary(),			%% 18 - ctrl_modified			-> Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
						  ctrl_hash :: non_neg_integer()		%% 19 - ctrl_hash 				-> Hash gerado para poder comparar dois registros	
          }).


-define(USER_PERMISSION_SCHEMA_DESCRIPTOR, {
			   atom_type,										%%  0 - nome da tabela	
			   non_neg_integer_type, 							%%  1 - id   
			   non_neg_integer_type,							%%  2 - user_id
			   non_neg_integer_type,							%%  3 - client_id
			   non_neg_integer_type,							%%  4 - perfil_id
			   non_neg_integer_type,							%%  5 - bash
			   non_neg_integer_type,							%%  6 - bash2
			   binary_type,										%%  7 - name
			   binary_type,										%%  8 - url
			   boolean_type,									%%  9 - grant_get
			   boolean_type,									%% 10 - grant_post
			   boolean_type,									%% 11 - grant_put
			   boolean_type,									%% 12 - grant_delete
			   non_neg_integer_type,							%% 13 - position
			   string_type,										%% 14 - ctrl_path
			   string_type,										%% 15 - ctrl_file
			   binary_type,										%% 16 - ctrl_insert
			   binary_type, 									%% 17 - ctrl_update
			   binary_type,										%% 18 - ctrl_modified
			   non_neg_integer_type								%% 19 - ctrl_hash 	
		}).


-record(user_perfil, {id :: non_neg_integer(), 				%%  1 - id				
					  user_id :: non_neg_integer(),			%%  2 - user_id			
					  client_id :: non_neg_integer(),		%%  3 - client_id		
					  name :: binary(), 					%%  4 - name
					  ctrl_path :: string(),				%%  5 - ctrl_path
					  ctrl_file :: string(),				%%  6 - ctrl_file
					  ctrl_insert :: binary(),				%%  7 - ctrl_insert				-> Data que foi inserido no banco mnesia
					  ctrl_update :: binary(), 				%%  8 - ctrl_update				-> Data que foi atualiado no banco mnesia			
					  ctrl_modified :: binary(),			%%  9 - ctrl_modified			-> Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
					  ctrl_hash :: non_neg_integer()		%% 10 - ctrl_hash 				-> Hash gerado para poder comparar dois registros	
		}).
          
-define(USER_PERFIL_SCHEMA_DESCRIPTOR, {
			   atom_type,									%%  0 - nome da tabela	
			   non_neg_integer_type, 						%%  1 - id   
			   non_neg_integer_type,						%%  2 - user_id
			   non_neg_integer_type,						%%  3 - client_id
			   binary_type,									%%  4 - name
			   string_type,									%%  5 - ctrl_path
			   string_type,									%%  6 - ctrl_file
			   binary_type,									%%  7 - ctrl_insert
			   binary_type, 								%%  8 - ctrl_update
			   binary_type,									%%  9 - ctrl_modified
			   non_neg_integer_type							%% 10 - ctrl_hash 	
		}).


-record(client, {id :: non_neg_integer(), 					%%  1 - id       				
				 name :: binary(), 							%%  2 - name
			     description :: binary(),					%%  3 - description
			     secret :: binary(),						%%  4 - secret
				 redirect_uri :: binary(),					%%  5 - redirect_uri
				 active :: boolean(),						%%  6 - active
				 scope :: binary(),							%%  7 - scope	
				 version :: binary(),						%%  8 - version
				 group :: binary(), 						%%  9 - group
				 glyphicon :: binary(),						%% 10 - glyphicon
			     ctrl_path :: string(),						%% 11 - ctrl_path
			     ctrl_file :: string(),						%% 12 - ctrl_file
			     ctrl_insert :: binary(),					%% 13 - ctrl_insert				-> Data que foi inserido no banco mnesia
			     ctrl_update :: binary(), 					%% 14 - ctrl_update				-> Data que foi atualiado no banco mnesia			
			     ctrl_modified :: binary(),					%% 15 - ctrl_modified			-> Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			     ctrl_hash :: non_neg_integer()				%% 16 - ctrl_hash 				-> Hash gerado para poder comparar dois registros	
		}).


-define(CLIENT_SCHEMA_DESCRIPTOR, {
			   atom_type,									%%  0 - nome da tabela	
			   non_neg_integer_type, 						%%  1 - id   
			   binary_type,									%%  2 - name
			   binary_type,									%%  3 - description
			   binary_type,									%%  4 - secret
			   binary_type,									%%  5 - redirect_uri
			   boolean_type,								%%  6 - active
			   binary_type,									%%  7 - scope
			   binary_type,									%%  8 - version
			   binary_type,									%%  9 - group
			   binary_type,									%% 10 - glyphicon
			   string_type,									%% 11 - ctrl_path
			   string_type,									%% 12 - ctrl_file
			   binary_type,									%% 13 - ctrl_insert
			   binary_type, 								%% 14 - ctrl_update
			   binary_type,									%% 15 - ctrl_modified
			   non_neg_integer_type							%% 16 - ctrl_hash 	
		}).

-record(ctrl_params, {name :: string(),
					  value
		}).



-record(service_datasource, {id :: non_neg_integer(),
							 rowid :: non_neg_integer(),
							 type :: atom(),								%% postgresql, sqlserver, csvfile, mnesia
							 driver :: binary(),							%% sqlite3, odbc, undefined
							 connection :: binary(),
							 table_name :: binary() | atom() | list(atom()),
							 fields :: binary() | atom() | list(atom()),
							 remap_fields :: map(),							%% Permite expor um campo com outro nome
							 remap_fields_rev :: map(),						
							 show_remap_fields :: boolean(),				%% Indica se deve mostrar os campos remapeados
							 primary_key :: binary() | atom(),
							 foreign_key :: binary() | atom(),
							 foreign_table_name  :: binary() | atom(),			
							 csv_delimiter :: binary(),
							 sql :: binary(),
							 timeout :: non_neg_integer(),
							 max_pool_size :: non_neg_integer(),
							 conn_ref,
							 pid_module,
							 pid_module_ref,
							 owner,
							 owner_ref,
							 connection_count_metric_name :: atom(),		%% Quantas conexões alocadas
							 connection_created_metric_name :: atom(),		%% Quantas conexões criadas
							 connection_closed_metric_name :: atom(),   	%% Quantas conexões foram fechadas de forma normal
							 connection_shutdown_metric_name :: atom(), 	%% Quantas conexões foram fechadas devido algum erro
							 connection_reuse_metric_name :: atom(), 		%% Quantas conexões foram reutilizadas
							 connection_unavailable_metric_name :: atom(), 	%% Quantas vezes não houve conexão
							 connection_max_pool_size_exceeded_metric_name :: atom(), 	%% Quantas vezes excedeu o número de conexões permitidos
							 sql_check_valid_connection :: string(),
							 check_valid_connection_timeout :: non_neg_integer(),
							 close_idle_connection_timeout :: non_neg_integer(),
							 ctrl_path :: string(),
							 ctrl_file :: string(),
							 ctrl_insert,									%% Data que foi inserido no banco mnesia
							 ctrl_update, 									%% Data que foi atualiado no banco mnesia			
							 ctrl_modified,									%% Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
							 ctrl_hash										%% Hash gerado para poder comparar dois registros
							}).


-record(service_owner, {  id :: non_neg_integer(),
						   name :: string(),
						   title :: string(),
						   comment :: string()
						}).


-record(service, {  id :: non_neg_integer(), 					%% Id do serviço
					rowid :: non_neg_integer(),					%% Identificador interno do contrato (utilizado para localizar o contrato)
					name :: binary(), 							%% Nome do contrato do serviço (Por default usa-se a própria URL como name)
					url :: string(),  							%% URL do contrato do serviço
					type :: binary(),							%% Verbo HTTP do contrato (GET, POST, PUT, DELETE e OPTIONS) ou KERNEL para módulos do barramento
					service :: binary(),						%% Serviço que será executado no contrato
					middleware :: atom(),						%% Miidleware definido para pós processamento do serviço
					module_name :: string(), 					%% Nome do módulo do serviço que vai atender a requisição. Ex.: br.erlangms.HelloWorldService  
					module_name_canonical :: string(), 			%% Nome do módulo canonico do serviço que vai atender a requisição. Ex.: HelloWorldService  
					module :: atom(),  							%% Atom do processo do módulo de serviço que vai atender a requisição
					function_name :: string(),					%% Nome da mensagem ou função que vai ser invocada no processo que vai atender a requisição
					function :: atom(),  						%% Atom da mensagem ou função que vai ser invocada no processo que vai atender a requisição
					use_re = false :: boolean(),				%% Flag que indica se usa expressão regular
					id_re_compiled = undefined, 				%% Identificador da expressão regular que vai verificar se a URL bate com a URL da requisição
					public = true :: boolean(), 				%% Indica se o contrato estará listado no Portal API Management
					comment :: binary(), 						%% Comentário sobre o que o contrato oferece em termos de serviço
					version :: binary(), 						%% Versão do contrato do serviço
					owner :: binary(),  						%% Quem é o proprietário pelo serviço. Ex.: auth
					group :: binary(),							%% Quem é o grupo do serviço. Ex.: auth/user
					async :: boolean(),							%% Indica se o serviço será processado em segundo plano (chamada assíncrona)
					querystring :: list(map()),					%% Definição da querystring para o contrato do serviço
					qtd_querystring_req :: non_neg_integer(), 	%% Indica quantas querystrings são obrigatórias
					host :: atom(),  							%% Atom do host onde está o módulo do serviço que vai processar a requisição
					host_name,				  					%% Nome do host onde está o módulo do serviço que vai processar a requisição
					result_cache :: non_neg_integer(), 			%% Indica quanto tempo em milisegundos o resultado vai ficar armazenado em cache
					authorization :: atom(),					%% Forma de autenticação (public, basic, oauth2)
					authorization_public_check_credential :: boolean(),		%% Faz a checagem da credencial do usuário quando o serviço é publico
					oauth2_with_check_constraint :: boolean(),
					oauth2_allow_client_credentials :: boolean(),
					oauth2_token_encrypt :: boolean(),
					auth_allow_user_inative_credentials :: boolean(),  % Permite login de usuários inativos.
					page,										%% Page django file
					page_module,								%% Page module django file compiled
					page_mime_type :: binary(),					%% Page mime type
					node,										%% Node ou lista de node onde os serviços estão publicados
					lang :: binary(),							%% Linguagem que foi utilizada para implementar o serviço
					datasource :: #service_datasource{},		%% Datasource para a fonte de dados
					debug = false :: boolean(),					%% Permite habilitar um modo debug (depende da implementação do serviço)
					schema_in :: non_neg_integer(),
					schema_out :: non_neg_integer(),
					pool_size :: non_neg_integer(),
					pool_max :: non_neg_integer(),
					timeout :: non_neg_integer(),				%% Tempo que o dispatcher aguarda em milisegundos o processamento de um serviço antes de retornar etimeout_service para o cliente
					timeout_alert_threshold :: non_neg_integer(),  	% Emite um alert no log após aguardar um determinado serviço por x milisegundos. O valor 0 (zero) desliga o threshold.
					log_show_response :: boolean(),		%% Se true, imprime o response no log
					log_show_payload :: boolean(),		%% Se true, imprime o payload no log
					expires :: non_neg_integer(),				%% Cabeçalho HTTP expires
					cache_control :: binary(),					%% Cabeçalho HTTP cache-control
					enable = false :: boolean(),
					content_type :: binary(),					%% Tipo de conteúdo (Ex.: application/json, application/pdf)
					path :: string(),							%% Local para carregar arquivos estáticos
					filename :: binary(),						%% Alguns serviços podem precisar informar um nome de arquivo
					redirect_url :: binary(),					%% redirect url						
					tcp_listen_address,
					tcp_listen_address_t,
					tcp_listen_prefix_interface_names :: list(string()),
					tcp_allowed_address,
					tcp_allowed_address_t,
					tcp_max_connections :: non_neg_integer(),
					tcp_port :: non_neg_integer(),
					tcp_is_ssl :: boolean(),
					tcp_ssl_cacertfile,
					tcp_ssl_certfile,
					tcp_ssl_keyfile,
					protocol :: binary(),
					properties :: map(),						%% Outros parâmetros
					ctrl_path :: string(),						%% Local de onde o catálogo foi carregado
					ctrl_file :: string(),						%% Nome do arquivo onde está especificado o catálogo
				    ctrl_insert,								%% Data que foi inserido no banco mnesia
					ctrl_update, 								%% Data que foi atualiado no banco mnesia			
					ctrl_modified,								%% Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
					ctrl_hash,									%% Hash gerado para poder comparar dois registros
					start_timeout :: non_neg_integer(),			%% Define um timeout inicial para o processo
					service_exec_metric_name :: atom(),
					service_result_cache_hit_metric_name :: atom(),
					service_host_denied_metric_name :: atom(),
					service_auth_denied_metric_name :: atom(),
					service_error_metric_name :: atom(),
					service_unavailable_metric_name :: atom(),
					service_timeout_metric_name :: atom(),
					service_resend_msg1 :: atom(),
					http_max_content_length :: non_neg_integer(),
					http_headers :: map(),
					restricted :: boolean(),					%% Serviços restrito para admins
					glyphicon :: binary(),						%% classe do glyphicon
					metadata :: binary()						%% Representação em json do que será enviado para o web service /catalog
				}).

			   
-record(request, {
					  rid  :: non_neg_integer(), 				%%  1 - rid    					Request ID (Identificador da requisição gerada automaticamente)
					  rowid :: non_neg_integer(),				%%  2 - rowid  					Identificador interno da requisição. Ver ems_util:hashsym_and_params
					  service :: #service{},					%%  3 - service					Contrato que estabelece o serviço que vai atender a requisição
					  timestamp :: calendar:date(), 			%%  4 - Timestamp 	
					  latency :: non_neg_integer(),				%%  5 - latency 				Tempo que levou para processar a requisição
					  code :: non_neg_integer(), 				%%  6 - code					Código de retorno HTTP (Ex.: 202 OK, 404 Não Encontrado)
					  reason :: atom(),							%%  7 - reason					Atom para indicar o erro ou status da requisição
					  reason_detail :: atom(),					%%  8 - reason_detail			Atom para indicar o erro ou status da requisição
					  reason_exception :: any(),				%%  9 - reason_exception		Registra a exception ocorrida em run time
					  type :: binary(),							%% 10 - type					Verbo HTTP (GET, POST, PUT, DELETE e OPTIONS)
					  operation :: atom(),						%% 11 - operation				Descreve a operação sendo realizada
					  uri :: binary(),							%% 12 - uri						URI da requisição do serviço
					  url :: string(),							%% 13 - url						URL da requisição do serviço
					  url_masked :: boolean(),					%% 14 - url_masked				Indica se a url está mascarada. Ex.: /erl.ms/L2F1dGgvY2xpZW50Lz9maWx0ZXI9InsgICJuYW1lIiA6ICJQb3N0bWFuIiB9Ig==
					  version :: string(),						%% 15 - version					Versão do cabeçalho HTTP
					  payload :: binary(),						%% 16 - payload					Corpo da requisição (aceita somente JSON)
					  payload_map :: map(),						%% 17 - payload_map				Corpo da requisição convertida para map após o parser e validação
					  querystring :: binary(),					%% 18 - querystring				Querystring da requisição
					  querystring_map :: map(),					%% 19 - querystring_map			Querystring convertida para map após o parser e validação
					  params_url :: map(),						%% 20 - params_url				Map com os parâmetros da URL
					  content_type_in :: binary(),				%% 21 - content_type_in			Tipo de conteúdo de entrada (Ex.: application/json)
					  content_type_out :: binary(),				%% 22 - content_type_out		Tipo de conteúdo de saída. (Ex.: application/json)
					  content_length :: non_neg_integer(), 		%% 23 - content_length			Largura da requisição
					  accept :: binary(),						%% 24 - accept					Parâmetro ACCEPT HTTP
					  user_agent :: binary(),					%% 25 - user_agent
					  user_agent_version :: binary(),			%% 26 - user_agent_version
					  accept_encoding :: binary(),				%% 27 - accept_encoding			Parâmetro ACCEPT_ENCODING HTTP
					  cache_control :: binary(),				%% 28 - cache_control			Parâmetro CACHE-CONTROL HTTP
					  etag :: binary(),							%% 29 - etag					Parâmetro ETag
					  if_modified_since :: binary(),			%% 30 - if_modified_since		Parâmetro If-Modified-Since
					  if_none_match :: binary(),			    %% 31 - if_none_match			Parâmetro If-None-Match
					  ip :: tuple(),							%% 32 - ip
					  ip_bin :: binary(),						%% 33 - ip_bin	
					  t1 :: non_neg_integer(),					%% 34 - t1						
					  authorization :: binary(),				%% 36 - authorization			Dados da autenticação da requisição
					  client :: #client{},						%% 37 - client
					  user :: #user{},							%% 38 - user			
					  node_exec :: pid(),						%% 39 - node_exec				Processo que executou a solicitação
					  worker_send :: pid(),						%% 40 - worker_send				Processo que solicitou a solicitação
					  status = req_processing :: atom(),		%% 41 - status					req_processing, req_done
					  protocol :: atom(),						%% 42 - protocol				Protocol (http, ldap)
					  protocol_bin :: binary(),					%% 43 - protocol_bin
					  port :: non_neg_integer(),				%% 44 - port		
					  result_cache = false :: boolean(),		%% 45 - result_cache
					  result_cache_rid :: non_neg_integer(),	%% 46 - result_cache_rid
					  response_data = <<>> :: binary(),			%% 47 - response_data
					  response_header = #{} :: map(),			%% 48 - response_header
					  req_hash :: non_neg_integer(),			%% 49 - req_hash				Hash gerado para comparar requisições. Função utilizada: erlang:phash2
					  host :: binary(),							%% 50 - host
					  filename :: string(),						%% 51 - filename				Qual arquivo foi lido do disco para requisições que leêm arquivos no disco
					  referer :: binary(),						%% 52 - referer
					  access_token :: binary(),					%% 53 - access_token
					  scope :: binary(),						%% 54 - scope
					  oauth2_grant_type :: binary(),			%% 55 - oauth2_grant_type
					  oauth2_access_token :: binary(),			%% 56 - oauth2_access_token
					  oauth2_refresh_token :: binary()			%% 57 - oauth2_refresh_token
				  }).


-record(ctrl_sqlite_table, {file_name :: string(), 
							last_modified :: file:date_time()}).
					

-record(catalog_schema, {id :: non_neg_integer(), 
						 name :: string(),	
						 description :: string(),
						 json_schema :: map()
						}).

-record(schema_type, {id :: non_neg_integer(), 
					  name :: string(),	
					  description :: string(),
					  json_schema :: map()
				}).


-record(stat_counter_hist, {  id :: non_neg_integer(),				%% 1 - id
							  stat_name :: atom(),					%% 2 - stat_name
							  stat_value :: non_neg_integer,		%% 3 - stat_value
							  stat_date :: binary(),				%% 4 - stat_date
							  stat_time :: binary(),				%% 5 - stat_time
							  stat_service_name :: binary(),		%% 6 - stat_service_name
							  stat_service_url :: binary(),			%% 7 - stat_service_url
							  stat_service_type :: binary(),		%% 8 - stat_service_type
							  stat_label :: binary()				%% 9 - stat_label
							}).


-define(STAT_COUNTER_HIST_DESCRIPTOR, {
			   atom_type,									%%  0 - nome da tabela	
			   non_neg_integer_type,						%%  1 - id
			   atom_type, 									%%  2 - stat_name   
			   non_neg_integer_type,						%%  3 - stat_value
			   binary_type,									%%  4 - stat_date
			   binary_type,									%%  5 - stat_time
			   binary_type,									%%  6 - stat_service_name
			   binary_type,									%%  7 - stat_service_url
			   binary_type,									%%  8 - stat_service_type
			   binary_type									%%  9 - stat_label
		}).


-record(auth_oauth2_access_token, { id :: binary(),
									context :: binary()
								  }).

-record(auth_oauth2_access_code, { id :: binary(),
								   context :: binary()
								 }).

-record(auth_oauth2_refresh_token, { id :: binary(),
									 context :: binary()
									}).

		
