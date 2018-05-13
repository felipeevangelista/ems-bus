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

-record(user, {id :: non_neg_integer(), 					%%  1 > id       				-> TB_Usuario.UsuId
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
			   ctrl_insert :: calendar:timestamp(),			%% 33 - ctrl_insert				-> Data que foi inserido no banco mnesia
			   ctrl_update :: calendar:timestamp(), 		%% 34 - ctrl_update				-> Data que foi atualiado no banco mnesia			
			   ctrl_modified :: calendar:timestamp(),		%% 35 - ctrl_modified			-> Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash :: non_neg_integer()				%% 36 - ctrl_hash 				-> Hash gerado para poder comparar dois registros	
		}).
		
-define(USER_DATA_TYPE, {
			   non_neg_integer_type, 				%%  1 > id   
			   non_neg_integer_type,				%%  2 - codigo
			   binary_type,							%%  3 - login	
			   binary_type, 						%%  4 - name		
			   binary_type,							%%  5 - cpf		
			   binary_type, 						%%  6 - email	
			   binary_type,							%%  7 - password 
			   binary_type,							%%  8 - dt_expire_password
			   non_neg_integer_type,				%%  9 - type
			   non_neg_integer_type,				%% 10 - subtype
			   binary_type,							%% 11 - passwd_crypto
			   non_neg_integer_type,				%% 12 - type_email
			   boolean_type,						%% 13 - active
			   binary_type,							%% 14 - endereco
			   binary_type,							%% 15 - complemento_endereco
			   binary_type,							%% 16 - bairro
			   binary_type,							%% 17 - cidade
			   binary_type,							%% 18 - uf
			   binary_type,							%% 19 - cep
			   binary_type,							%% 20 - rg	
			   binary_type,							%% 21 - data_nascimento
			   non_neg_integer_type,				%% 22 - sexo
			   binary_type,							%% 23 - telefone
			   binary_type,							%% 24 - celular
			   binary_type,							%% 25 - ddd
			   binary_type,							%% 26 - nome_pai
			   binary_type,							%% 27 - nome_ae
			   non_neg_integer_type,				%% 28 - nacionalidade
			   non_neg_integer_type,				%% 29 - remap_user_id
			   boolean_type,						%% 30 - admin	
			   string_type,							%% 31 - ctrl_path
			   string_type,							%% 32 - ctrl_file
			   timestamp_type,						%% 33 - ctrl_insert
			   timestamp_type, 						%% 34 - ctrl_update
			   timestamp_type,						%% 35 - ctrl_modified
			   non_neg_integer_type					%% 36 - ctrl_hash 	
			}).		
		
%
% Muitos atributos são armazenados no histórico para histórico pois na tabelas origem tais atributos podem mudar
%
-record(user_history, {
			   id :: non_neg_integer(), 						%% identificador do history
			   user_id :: non_neg_integer(), 					%% identificador do user
			   user_codigo :: non_neg_integer(),				%% código da pessoa
			   user_login :: binary(),							%% login do usuário
			   user_name :: binary(), 							%% nome do usuário
			   user_cpf :: binary(),
			   user_email :: binary(), 							
			   user_type :: non_neg_integer(),					%% veja record user
			   user_subtype :: non_neg_integer(),				%% veja record user
			   user_type_email :: non_neg_integer(),			%% undefined = desconhecido  1 = Institucional  2 = Pessoal
			   user_active :: boolean(),
			   user_admin :: boolean(),							%% o user eh admin
			   client_id :: non_neg_integer(), 					%% identificador do client
			   client_name :: binary(), 
			   service_rowid :: non_neg_integer(), 				%% rowid do catálogo
			   service_name :: binary(), 						%% Nome do contrato do serviço
			   service_url :: string(),  						%% URL do contrato do serviço
			   service_type :: binary(),						%% Verbo HTTP do contrato (GET, POST, PUT, DELETE e OPTIONS) ou KERNEL para módulos do barramento
			   service_service :: binary(),						%% Serviço que será executado no contrato
			   service_use_re :: boolean(),						%% Flag que indica se usa expressão regular
			   service_public :: boolean(), 					%% Identificador da expressão regular que vai verificar se a URL bate com a URL da requisição
			   service_version :: binary(), 					%% Versão do serviço executado
			   service_owner :: binary(),  						%% Quem é o proprietário pelo serviço
			   service_group :: binary(),  						%% Quem é o grupo do serviço
			   owner :: binary(),  								%% Quem é o proprietário pelo serviço. Ex.: auth
			   service_async :: boolean(),						%% Indica se o serviço será processado em segundo plano (chamada assíncrona)
			   request_rid,       								%% Request ID (Identificador da requisição gerada automaticamente)
			   %request_latency :: non_neg_integer(),			%% Latência (tempo que levou para processar a requisição)
			   request_type :: binary(),						%% Verbo HTTP (GET, POST, PUT, DELETE e OPTIONS)
			   request_uri :: binary(),							%% URI da requisição do serviço
			   request_url :: binary(),							%% URL da requisição do serviço
			   request_url_masked :: boolean(),					%% Indica se a url está mascarada. Ex.: /erl.ms/L2F1dGgvY2xpZW50Lz9maWx0ZXI9InsgICJuYW1lIiA6ICJQb3N0bWFuIiB9Ig==
			   request_http_version :: binary(),				%% Versão do cabeçalho HTTP
			   %request_payload :: binary(),					%% Payload da requisição (se menor que 4K)
			   request_querystring :: binary(),					%% Querystring da requisição
			   %request_params_url :: binary(),					%% Map com os parâmetros da URL
			   request_content_type_in :: binary(),				%% Tipo de conteúdo de entrada (Ex.: application/json)
			   request_content_type_out :: binary(),			%% Tipo de conteúdo de saída. (Ex.: application/json)
			   request_content_length :: non_neg_integer(), 	%% Largura da requisição
			   request_accept :: binary(),						%% Parâmetro ACCEPT HTTP
			   request_user_agent :: binary(),					%% Nome do browser
			   request_user_agent_version :: binary(),			%% Versão do browser
			   request_t1,										%% Utilizado para cálculo da latência (Tempo inicial em milisegundos)
			   request_authorization :: binary(),				%% Dados da autenticação da requisição
			   request_port :: non_neg_integer(),				
			   %request_response_data,
			   request_bash,
			   request_host :: binary(),						%% Ip do barramento
			   request_filename :: string(),					%% Qual arquivo foi lido do disco
			   request_referer :: binary(),
			   request_access_token :: binary(),
			   request_operation :: atom(),						%% Descreve melhor a operação sendo realizada
			   request_reason_detail :: atom(),					%% Registra a constante da mensagem de erro detalhado, quando status indicar um erro
			   request_reason :: atom(),						%% Registra a constante da mensagem de erro, quando status indicar um erro
			   request_code :: non_neg_integer(),	 			%% Código de retorno HTTP (Ex.: 202 OK, 404 Não Encontrado)
			   request_protocol :: atom(),						%% Protocol (http, ldap)
   			   request_timestamp :: binary()					%% Timestamp de quando que a requisição ocorreu

		}).
		
-record(user_dados_funcionais, {
			   id :: non_neg_integer(), 					%% %% identificador dos dados funcionais (Na UnB é o campo Tb_Usuario.UsuId)
			   type :: non_neg_integer(),					%% 0 = interno  1 = tecnico  2 = docente  3 = discente
			   subtype :: non_neg_integer(),				%% se aluno,  1 = extensao 2 = graduacao 3 = aperfeicoamento 4 = especializacao 5 = mestrado 6 = doutorado 7 = pos-doutorado 8 = residencia 9 = aluno especial - graduacao 10 = aluno especial - pos-graduacao 11 = estagio em pos-graduacao
			   active :: boolean(),
			   matricula :: non_neg_integer(),				%% matrícula proveniente de dados funcionais
			   ctrl_path :: string(),
			   ctrl_file :: string(),
			   ctrl_insert,									%% Data que foi inserido no banco mnesia
			   ctrl_update, 								%% Data que foi atualiado no banco mnesia			
			   ctrl_modified,								%% Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash									%% Hash gerado para poder comparar dois registros
		}).

-record(user_email, {
			   id :: non_neg_integer(), 					%% identificador dos email (Na UnB é o campo TB_Email.EmaCodigo)
			   codigo :: non_neg_integer(),					%% código da pessoa. (Na UnB é o campo Tb_Pessoa.PesCodigoPessoa)
			   email :: binary(),	
			   type :: non_neg_integer(),					%% 1 = institucional  2 = outro
			   ctrl_path :: string(),
			   ctrl_file :: string(),
			   ctrl_insert,									%% Data que foi inserido no banco mnesia
			   ctrl_update, 								%% Data que foi atualiado no banco mnesia			
			   ctrl_modified,								%% Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash									%% Hash gerado para poder comparar dois registros
		}).

-record(user_endereco, {
			   id :: non_neg_integer(), 					%% identificador do endereço (Na UnB é o campo TB_Endereco.EndCodigo)
			   codigo :: non_neg_integer(),					%% código da pessoa. (Na UnB é o campo Tb_Pessoa.PesCodigoPessoa)
			   endereco :: binary(),
			   complemento :: binary(),
			   bairro :: binary(),
			   cidade :: binary(),
			   uf :: binary(),
			   cep :: binary(),
			   type :: non_neg_integer(),					%% 1 = residencial  2 = comercial 3 = exterior 4 = outro
			   ctrl_path :: string(),
			   ctrl_file :: string(),
			   ctrl_insert,									%% Data que foi inserido no banco mnesia
			   ctrl_update, 								%% Data que foi atualiado no banco mnesia			
			   ctrl_modified,								%% Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash									%% Hash gerado para poder comparar dois registros
		}).

-record(user_telefone, {
			   id :: non_neg_integer(), 					%% identificador do endereço (Na UnB é o campo TB_Telefone.TelCodigo)
			   codigo :: non_neg_integer(),					%% código da pessoa. (Na UnB é o campo Tb_Pessoa.PesCodigoPessoa)
			   numero :: binary(),
			   ramal :: non_neg_integer(),
			   ddd :: binary(),
			   type :: non_neg_integer(),					%% 1 = celular  2 = comercial 3 = residencial
			   ctrl_path :: string(),
			   ctrl_file :: string(),
			   ctrl_insert,									%% Data que foi inserido no banco mnesia
			   ctrl_update, 								%% Data que foi atualiado no banco mnesia			
			   ctrl_modified,								%% Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
			   ctrl_hash									%% Hash gerado para poder comparar dois registros
		}).

-record(user_permission, {id :: non_neg_integer(),				%% identificador do perfil (required) (Na UnB é o campo TB_Perfil_Transacao.PTrid)
						  user_id :: non_neg_integer(),			%% identificador do usuário (required) (Na UnB é o campo Tb_Usuario.UsuId)
						  client_id :: non_neg_integer(),		%% identificador do cliente (required) (Na UnB é o campo Tb_Sistemas.PerSisId)
						  perfil_id :: non_neg_integer(),		%% identificador do perfil  (required) (Na UnB é o campo Tb_Perfil.PerId)
						  hash :: non_neg_integer(),
						  hash2 :: non_neg_integer(),
						  name :: binary(),
						  url :: binary(),
						  grant_get :: boolean(),
						  grant_post :: boolean(),
						  grant_put :: boolean(),
						  grant_delete :: boolean(),
						  position :: non_neg_integer(),
						  ctrl_path :: string(),
						  ctrl_file :: string(),
						  ctrl_insert,							%% Data que foi inserido no banco mnesia
						  ctrl_update, 							%% Data que foi atualiado no banco mnesia			
						  ctrl_modified,						%% Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
						  ctrl_hash								%% Hash gerado para poder comparar dois registros
          }).


-record(user_perfil, {id :: non_neg_integer(), 				%% identificador do perfil (required) (Na UnB é o campo Tb_Perfil.PerId)				
					  user_id :: non_neg_integer(),			%% identificador interno do usuário (required)
					  client_id :: non_neg_integer(),		%% identificador interno do client (required)
					  name :: binary(), 					%% nome do perfil (required)
					  ctrl_path :: string(),
				      ctrl_file :: string(),
				      ctrl_insert,							%% Data que foi inserido no banco mnesia
				      ctrl_update, 							%% Data que foi atualiado no banco mnesia			
				      ctrl_modified,						%% Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
				      ctrl_hash								%% Hash gerado para poder comparar dois registros
		}).
          

-record(client, {id :: non_neg_integer(), 					
				 name :: binary(), 
			     description :: binary(),
			     secret :: binary(),
				 redirect_uri :: binary(),
				 active :: boolean(),
				 scope :: binary(),
				 version :: binary(),
				 group :: binary(), 						%% Quem é o grupo do client
				 glyphicon :: binary(),						%% classe do glyphicon
				 ctrl_path :: string(),
				 ctrl_file :: string(),
				 ctrl_insert,								%% Data que foi inserido no banco mnesia
				 ctrl_update, 								%% Data que foi atualiado no banco mnesia			
				 ctrl_modified,								%% Data que foi modificado na fonte onde está cadastrado (em disco ou banco de dados externo)
				 ctrl_hash									%% Hash gerado para poder comparar dois registros
		}).


-record(ctrl_params, {name :: string(),
					  value
		}).

			   
-record(request, {
					  rid,       								%% Request ID (Identificador da requisição gerada automaticamente)
					  rowid,									%% Identificador interno da requisição. Ver ems_util:hashsym_and_params
					  service,   								%% Contrato que estabelece o serviço que vai atender a requisição
					  timestamp, 								%% Timestamp de quando que a requisição ocorreu
					  latency :: non_neg_integer(),				%% Latência (tempo que levou para processar a requisição)
					  code :: non_neg_integer(), 				%% Código de retorno HTTP (Ex.: 202 OK, 404 Não Encontrado)
					  reason :: atom(),							%% Registra uma constante para indicar o erro ou status da requisição
					  reason_detail :: atom(),					%% Registra uma 2 constante para indicar o erro ou status da requisição
					  reason_exception :: any(),				%% Registra a exception ocorrida em run time
					  type :: binary(),							%% Verbo HTTP (GET, POST, PUT, DELETE e OPTIONS)
					  operation :: atom(),						%% Descreve a operação sendo realizada
					  uri :: binary(),							%% URI da requisição do serviço
					  url :: string(),							%% URL da requisição do serviço
					  url_masked :: boolean(),					%% Indica se a url está mascarada. Ex.: /erl.ms/L2F1dGgvY2xpZW50Lz9maWx0ZXI9InsgICJuYW1lIiA6ICJQb3N0bWFuIiB9Ig==
					  version :: string(),						%% Versão do cabeçalho HTTP
					  payload :: binary(),						%% Corpo da requisição (aceita somente JSON)
					  payload_map :: map(),						%% Corpo da requisição convertida para map após o parser e validação
					  querystring :: binary(),					%% Querystring da requisição
					  querystring_map :: map(),					%% Querystring convertida para map após o parser e validação
					  params_url :: map(),						%% Map com os parâmetros da URL
					  content_type_in :: binary(),				%% Tipo de conteúdo de entrada (Ex.: application/json)
					  content_type_out :: binary(),				%% Tipo de conteúdo de saída. (Ex.: application/json)
					  content_length :: non_neg_integer(), 		%% Largura da requisição
					  accept :: binary(),						%% Parâmetro ACCEPT HTTP
					  user_agent :: binary(),					%% Nome do browser
					  user_agent_version :: binary(),			%% Versão do browser
					  accept_encoding :: string(),				%% Parâmetro ACCEPT_ENCODING HTTP
					  cache_control :: binary(),				%% Parâmetro CACHE-CONTROL HTTP
					  etag :: string(),							%% Parâmetro ETag
					  if_modified_since :: string(),			%% Parâmetro If-Modified-Since
					  if_none_match :: string(),			    %% Parâmetro If-None-Match
					  ip :: tuple(),
					  ip_bin :: binary(),						%% Peer que iniciou a requisição
					  t1,										%% Utilizado para cálculo da latência (Tempo inicial em milisegundos)
					  worker :: pid(),							%% Processo worker http que vai atender a requisição
					  authorization :: binary(),				%% Dados da autenticação da requisição
					  client :: #client{},
					  user :: #user{},							%% Usuário da requisição ou public
					  node_exec = undefined,					%% Node que foi enviado a solicitação
					  worker_send,
					  status = req_processing :: atom(),		%% status: req_processing, req_done
					  protocol :: atom(),						%% Protocol (http, ldap)
					  protocol_bin :: binary(),	
					  port :: non_neg_integer(),				
					  result_cache = false :: boolean(),
					  result_cache_rid,
					  response_data = <<>>,
					  response_header = #{},
					  req_hash :: non_neg_integer(),			%% Hash gerado para comparar requisições. Função utilizada: erlang:phash2
					  host :: binary(),							%% Ip do barramento
					  filename :: string(),						%% Qual arquivo foi lido do disco
					  referer :: binary(),
					  access_token :: binary(),
					  scope :: binary(),
					  oauth2_grant_type :: binary(),
					  oauth2_access_token :: binary(),
					  oauth2_refresh_token :: binary()
					  
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


-record(produto, {id :: non_neg_integer(), 
				  name :: string(), 
				  price :: non_neg_integer()}).

-record(stat_counter_hist, {  id :: non_neg_integer(),
							  stat_name :: atom(),
							  stat_value :: non_neg_integer,
							  stat_date :: binary(),
							  stat_time :: binary(),
							  stat_service_name :: binary(),
							  stat_service_url :: binary(),
							  stat_service_type :: binary(),
							  stat_label :: binary()
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

		
