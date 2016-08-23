/*
This file aims to set up the sqlite3 database
to save the http_request and the corresponding
sqlite queries,file diff and session diff trees

author: Simon Koch <s9sikoch@stud.uni-saarland.de>
*/

CREATE TABLE selenese_commands (
       id               integer,
       tcname           character varying,
       command          character varying,
       target           character varying,
       value            character varying,
       PRIMARY KEY (id)
);

CREATE TABLE http_requests (
       id               integer,
       command_id       integer,
       time             character varying,
       request_url      character varying,
       request_body     character varying,
       header           character varying,
       method_type      character varying,       
       cookies          character varying,
       status_code      character varying NULL,
       PRIMARY KEY (id)
);

CREATE TABLE http_responses (
       id               integer,
       req_id           integer,
       time             character varying,
       status_code      character varying,
       header           character varying, 
       content          character varying,
       PRIMARY KEY (id)
);

CREATE TABLE xdebug_dumps (
       http_request_id  integer,
       dump_content     character varying,
       PRIMARY KEY (http_request_id)
);


CREATE TABLE sessions (
       http_request_id  integer,
       session_name     character varying,
       session_string   character varying,
       PRIMARY KEY(http_request_id,session_name),
       FOREIGN KEY(http_request_id)
       REFERENCES http_request(id)      
);
