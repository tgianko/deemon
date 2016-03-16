/*
This file aims to set up the sqlite3 database
to save the http_request and the corresponding
sqlite queries

autor: Simon Koch <s9sikoch@stud.uni-saarland.de>
*/
CREATE TABLE http_requests (
       id    integer,
       time  character varying,
       request character varying,
       PRIMARY KEY (id)
);


CREATE TABLE sql_query_types (
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
       PRIMARY KEY (http_request_id, query_counter),
       FOREIGN KEY (http_request_id)
       REFERENCES http_request(id),
       FOREIGN KEY (query_type)
       REFERENCES sql_query_type(type_name)
);


INSERT INTO sql_query_types VALUES
('INSERT','INSERT','represents an INSERT query'),
('UPDATE','UPDATE','represents an UPDATE query'),
('SELECT','SELECT','represents an SELECT query'),
('CREATE','CREATE','represents an CREATE query'),
('ALTER','ALTER','represents an ALTER query'),
('DROP','DROP','represents an DROP query');


