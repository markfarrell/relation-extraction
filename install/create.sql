/** 
 * @name create
 * @author Mark Farrell
 * @description requires the tables for this project.
 **/

-- TODO: 
-- OR conditions for actions: multiple entries, same value
-- AND conditions: conditions point to conditions

CREATE TABLE IF NOT EXISTS topics (
    id text primary key -- Unique values, use as primary key
);

CREATE TABLE IF NOT EXISTS conditions ( 
    id serial primary key,
    value text,
    topic_id text CHECK (topic_id IS NOT NULL),
    FOREIGN KEY (topic_id) REFERENCES topics(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS actions (
    id serial primary key,
    value text, 
    condition_id integer,
    topic_id text,
    FOREIGN KEY (condition_id) REFERENCES conditions (id) ON DELETE CASCADE,
    FOREIGN KEY (topic_id) REFERENCES topics (id) ON DELETE CASCADE,
    CHECK ((condition_id IS NOT NULL AND topic_id IS NULL) OR (condition_id IS NULL AND topic_id IS NOT NULL)) -- XOR constraint 
);

CREATE TABLE IF NOT EXISTS dependencies ( 
    id serial primary key,
    value text,
    action_id integer CHECK (action_id IS NOT NULL),
    topic_id text, -- Point to a topic this dependency relies on 
    FOREIGN KEY (action_id) REFERENCES actions(id) ON DELETE CASCADE,
    FOREIGN KEY (topic_id) REFERENCES topics(id) ON DELETE CASCADE
);
 
