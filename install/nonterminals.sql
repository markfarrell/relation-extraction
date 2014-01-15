/**
 * @name Nonterminals 
 * @author Mark Farrell
 * @description 
 * 
 * Nodes point to a parent, which can be 'NULL' for the root. 
 * This strategy permits storing multiple children for a given node.
 * 
 * Leaf nodes have a 'terminalid', pointing to
 * an entry in the table 'terminals', where the value for 
 * the node is stored. 
 **/

CREATE TABLE IF NOT EXISTS nonterminals ( 
    id INT UNSIGNED NOT NULL AUTO_INCREMENT,
    symbol VARCHAR(3) NOT NULL,
    terminalid INT UNSIGNED NULL,
    parent INT UNSIGNED NULL,
    PRIMARY KEY (id)
);

