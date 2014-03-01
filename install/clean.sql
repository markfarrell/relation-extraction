/** 
 * @name clean
 * @author Mark Farrell
 * @description purges the database.
 **/

DROP TABLE IF EXISTS topics CASCADE;
DROP TABLE IF EXISTS conditions CASCADE;
DROP TABLE IF EXISTS actions CASCADE;
DROP TABLE IF EXISTS dependencies CASCADE;


