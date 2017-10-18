/*
 This file is part of Deemon.

 Deemon is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 Deemon is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with Deemon.  If not, see <http://www.gnu.org/licenses/>.
*/

CREATE TABLE CSRF_tests_results (
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
       query_message    character varying, 
       query_hash       character varying, 
       apt_uuid         character varying, 
       observed         character varying, 
       tr_pattern       character varying,
       PRIMARY KEY (id)
);
