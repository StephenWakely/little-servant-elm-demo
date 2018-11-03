DROP TABLE IF EXISTS users;

CREATE TABLE users (
       id INTEGER PRIMARY KEY,
       username TEXT,
       age INTEGER,
       email TEXT
);

INSERT INTO users (username, age, email) VALUES ("Isaac", 375, "isaac@newton.com");
INSERT INTO users (username, age, email) VALUES ("Albert", 134, "ae@mc2.com");


