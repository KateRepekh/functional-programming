cd lab1
<br>
mysql -u root [-p] -e "CREATE DATABASE faculty_classrooms_db;"
<br>
mysql -u root [-p] -e "CREATE USER 'classrooms_db_user'@'localhost' IDENTIFIED BY ''"
<br>
mysql -u root [-p] -e "GRANT ALL PRIVILEGES ON faculty_classrooms_db.* TO 'classrooms_db_user'@'localhost'"
<br>
mysql -u root [-p] -e "FLUSH PRIVILEGES"
<br>
mysql -h "localhost" -u "classrooms_db_user" "faculty_classrooms_db" < "sql_queries/create_tables.sql"
<br>
mysql -h "localhost" -u "classrooms_db_user" "faculty_classrooms_db" < "sql_queries/fill_tables.sql"
