;;autor: Simon Koch <s9sikoch@stud.uni-saarland.de>

CREATE TABLE http_request (
       id    integer,
       time  integer,
       request character varying,
       PRIMARY KEY (id)
);


CREATE TABLE sql_query_type (
       type_name	    character_varying,
       type_keyword	    character_varying,
       type_description	    character_varying,
       PRIMARY KEY(type_name)
);


CREATE TABLE sql_queries (
       http_request_id	 integer,       
       query_counter	 integer,
       query_type	 character varying,
       query_string	 character varying,
       PRIMARY KEY(http_request_id,query_counter),
       FOREIGN KEY http_request_id
       REFERENCES http_request(id),
       FOREIGN KEY query_type
       REFERENCES sql_query_type(type_name)
);
