/** 
 * @name create
 * @author Mark Farrell
 * @description requires the tables for this project.
 **/

CREATE SCHEMA IF NOT EXISTS beagle;

-- TODO: 
-- OR conditions for actions: multiple entries, same value
-- AND conditions: conditions point to conditions

CREATE TABLE IF NOT EXISTS beagle.topics (
    id serial primary key, 
    value text UNIQUE,
    color integer DEFAULT 0 -- Colors are always stored as a hex value 
);

CREATE TABLE IF NOT EXISTS beagle.conditions ( 
    id serial primary key,
    value text,
    color integer DEFAULT 0, 
    topic_value text CHECK (topic_value IS NOT NULL),
    FOREIGN KEY (topic_value) REFERENCES beagle.topics(value) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS beagle.actions (
    id serial primary key,
    value text, 
    color integer DEFAULT 0, 
    condition_id integer,
    topic_value text,
    FOREIGN KEY (condition_id) REFERENCES beagle.conditions (id) ON DELETE CASCADE,
    FOREIGN KEY (topic_value) REFERENCES beagle.topics (value) ON DELETE CASCADE,
    CHECK ((condition_id IS NOT NULL AND topic_value IS NULL) OR (condition_id IS NULL AND topic_value IS NOT NULL)) -- XOR constraint 
);

CREATE TABLE IF NOT EXISTS beagle.dependencies ( 
    id serial primary key,
    value text,
    color integer DEFAULT 0, 
    action_id integer CHECK (action_id IS NOT NULL),
    topic_value text, -- Point to a topic this dependency relies on 
    FOREIGN KEY (action_id) REFERENCES beagle.actions(id) ON DELETE CASCADE,
    FOREIGN KEY (topic_value) REFERENCES beagle.topics(value) ON DELETE CASCADE
);
 
