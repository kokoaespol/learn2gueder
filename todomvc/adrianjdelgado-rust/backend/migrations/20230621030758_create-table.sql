CREATE TABLE todos (
    id        BLOB PRIMARY KEY NOT NULL,
    task      TEXT             NOT NULL,
    completed BOOLEAN          NOT NULL DEFAULT 0
);
