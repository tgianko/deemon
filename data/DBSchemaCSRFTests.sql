CREATE TABLE CSRF_tests (
       id               integer,
       seq_id           integer,
       time             character varying,
       projname         character varying,
       session          character varying,
       operation        character varying,
       user             character varying,
       uuid_request     character varying,
       uuid_tn          character varying,
       uuid_src_var     character varying,
       uuid_sink_var    character varying,
       method           character varying,
       url              character varying,
       headers          character varying,
       body             character varying,      
       PRIMARY KEY (id)
);

CREATE TABLE http_responses (
       id               integer,
       req_id           integer,
       time             character varying,
       status_code      character varying,
       headers          character varying, 
       content          character varying,
       PRIMARY KEY (id)
);