/**
 * @name Terminals 
 * @author Mark Farrell
 * @description Let nodes without children point to an entry in this table.
 * That way, if the same terminal is referenced in multiple trees their relationship can be quantified. 
 **/

CREATE TABLE IF NOT EXISTS terminals ( 
    id INT UNSIGNED NOT NULL AUTO_INCREMENT,
    symbol VARCHAR(3) NOT NULL,
    value VARCHAR(45) NOT NULL, /** Longest word in the english dictionary. **/
    PRIMARY KEY (id)
);


/** TODO: nonterminals -> point to parent, have terminal which can be NULL. **/
