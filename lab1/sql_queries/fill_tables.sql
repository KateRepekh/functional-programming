INSERT INTO faculty_users(status, username, password, first_name, last_name) 
            VALUES("Admin", "admin", "admin", "admin", "admin");
INSERT INTO faculty_users(status, username, password, first_name, last_name) 
VALUES("Teacher", "teacher1", "pass", "First", "Teacher");
INSERT INTO faculty_users(status, username, password, first_name, last_name) 
VALUES("Teacher", "teacher2", "pass", "Second", "Teacher");
INSERT INTO faculty_users(status, username, password, first_name, last_name) 
VALUES("Student", "student1", "pass", "First", "Student");
INSERT INTO faculty_users(status, username, password, first_name, last_name) 
VALUES("Student", "student2", "pass", "Second", "Student");

INSERT INTO classrooms(capacity, floor) VALUES(2, 1);
INSERT INTO classrooms(capacity, floor) VALUES(50, 1);
INSERT INTO classrooms(capacity, floor) VALUES(100, 2);

INSERT INTO seats(classroom_id, material) VALUES(1, 'Wood');
INSERT INTO seats(classroom_id, material) VALUES(1, 'Cotton');