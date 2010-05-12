package recording.neo4j.models;

import org.neo4j.graphdb.RelationshipType;

// Scala can't produce Java-compatible enums, so they will have to be
// defined in Java for now.

/**
 * Enum of all the possible relationships between nodes known at compile-time.
 */
public enum Predicates implements RelationshipType {
    ISPARENTOF
}
