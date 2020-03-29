DROP TABLE IF EXISTS users_to_lessons;
DROP TABLE IF EXISTS lessons;
DROP TABLE IF EXISTS faculty_classes;
DROP TABLE IF EXISTS seats;
DROP TABLE IF EXISTS classrooms;
DROP TABLE IF EXISTS faculty_users;

CREATE TABLE classrooms (
    classroom_id INT AUTO_INCREMENT NOT NULL,
    capacity MEDIUMINT NOT NULL,
    floor TINYINT NOT NULL,
    PRIMARY KEY (classroom_id)
);

CREATE TABLE seats (
    seat_id INT AUTO_INCREMENT NOT NULL,
    classroom_id INT NOT NULL,
    material ENUM('Wood', 'Plastic', 'Cotton', 'Leather', 'Vynil', 'Metal') NOT NULL,
    PRIMARY KEY (seat_id),
    FOREIGN KEY (classroom_id) REFERENCES classrooms(classroom_id) ON DELETE CASCADE
);

CREATE TABLE faculty_users (
    user_id INT AUTO_INCREMENT NOT NULL,
    status ENUM('Admin', 'Teacher', 'Student') NOT NULL,
    username VARCHAR(30) NOT NULL UNIQUE,
    password VARCHAR(50) NOT NULL,
    first_name VARCHAR(30) NOT NULL,
    last_name VARCHAR(50) NOT NULL,
    PRIMARY KEY (user_id)
);

CREATE TABLE faculty_classes (
    class_id INT AUTO_INCREMENT NOT NULL,
    teacher_id INT NOT NULL,
    class_name VARCHAR(100) NOT NULL,
    PRIMARY KEY (class_id),
    FOREIGN KEY (teacher_id) REFERENCES faculty_users(user_id) ON DELETE CASCADE
);

CREATE TABLE lessons (
    lesson_id INT AUTO_INCREMENT NOT NULL,
    class_id INT NOT NULL,
    classroom_id INT NOT NULL,
    day_of_week ENUM('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday') NOT NULL,
    time_slot ENUM('7-8', '8-9', '9-10', '10-11', '11-12', '12-13',
                   '13-14', '14-15', '15-16', '16-17', '17-18', '18-19', '19-20') NOT NULL,
    FOREIGN KEY (class_id) REFERENCES faculty_classes(class_id) ON DELETE CASCADE,
    FOREIGN KEY (classroom_id) REFERENCES classrooms(classroom_id) ON DELETE CASCADE,
    PRIMARY KEY (lesson_id),
    UNIQUE KEY (classroom_id, day_of_week, time_slot)
);

CREATE TABLE users_to_lessons (
    user_to_lesson_id INT AUTO_INCREMENT NOT NULL,
    lesson_id INT NOT NULL,
    user_id INT NOT NULL,
    seat_id INT NOT NULL,
    FOREIGN KEY (lesson_id) REFERENCES lessons(lesson_id) ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES faculty_users(user_id) ON DELETE CASCADE,
    FOREIGN KEY (seat_id) REFERENCES seats(seat_id) ON DELETE CASCADE,
    PRIMARY KEY (user_to_lesson_id),
    UNIQUE KEY (lesson_id, user_id),
    UNIQUE KEY (lesson_id, seat_id)
);

