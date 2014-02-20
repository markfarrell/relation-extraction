/** 
 * @name create
 * @author Mark Farrell
 * @description requires the database and its tables for this project.
 **/

CREATE DATABASE IF NOT EXISTS beagle;

-- TODO: 
-- OR conditions for actions: multiple entries, same value
-- AND conditions: conditions point to conditions

CREATE TABLE IF NOT EXISTS topics (
    id integer PRIMARY KEY,
    value text, 
    dependency_id integer,
    FOREIGN KEY (dependency_id) REFERENCES dependencies (id) ON DELETE CASCADE 
)


CREATE TABLE IF NOT EXISTS actions (
    id integer PRIMARY KEY,
    value text, 
    condition_id integer,
    topic_id integer,
    FOREIGN KEY (condition_id) REFERENCES conditions (id) ON DELETE CASCADE
    FOREIGN KEY (topic_id) REFERENCES topics (id) ON DELETE CASCADE
    CONSTRAINT CHECK ((condition_id IS NOT NULL AND topic_id IS NULL) OR (condition_id IS NULL AND topic_id IS NOT NULL)) -- XOR constraint 
)

CREATE TABLE IF NOT EXISTS conditions ( 
    id integer PRIMARY KEY,
    value text,
    topic_id integer,
    FOREIGN KEY (topic_id) REFERENCES topics(id) ON DELETE CASCADE
    CONSTRAINT CHECK (topic_id IS NOT NULL)
)

CREATE TABLE IF NOT EXISTS dependencies ( 
    id integer PRIMARY KEY,
    value text,
    action_id integer, 
    FOREIGN KEY (action_id) REFERENCES actions (id) ON DELETE CASCADE
    CONSTRAINT CHECK (action_id IS NOT NULL)
) 
 
