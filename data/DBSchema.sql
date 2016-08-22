/*
This file aims to set up the sqlite3 database
to save the http_request and the corresponding
sqlite queries,file diff and session diff trees

autor: Simon Koch <s9sikoch@stud.uni-saarland.de>
*/
CREATE TABLE http_requests (
       id    		   integer,
       time		   character varying,
       request_url 	   character varying,
       request_body 	   character varying,
       header 		   character varying,
       method_type 	   character varying,       
       cookies		   character varying,
       status_code 	   character varying NULL,
       PRIMARY KEY (id)
);


CREATE TABLE sql_queries (
       http_request_id	 integer,
       query_counter	 integer,
       query_string	 character varying,
       PRIMARY KEY (http_request_id, query_counter),
       FOREIGN KEY (http_request_id)
       REFERENCES http_request(id)
);


CREATE TABLE changed_files (
       http_request_id 	   integer,
       file_path	   character varying,
       PRIMARY KEY(http_request_id,file_path),
       FOREIGN KEY(http_request_id)
       REFERENCES http_request(id)
);


CREATE TABLE session_diff_trees (
       http_request_id		integer,
       session_id		character varying,
       diff_tree		character varying,
       PRIMARY KEY(http_request_id,session_id),
       FOREIGN KEY(http_request_id)
       REFERENCES http_request(id)
);

CREATE TABLE sessions (
       http_request_id		integer,
       session_id		character varying,
       session_string		character varying,
       PRIMARY KEY(http_request_id,session_id),
       FOREIGN KEY(http_request_id)
       REFERENCES http_request(id)      
);
