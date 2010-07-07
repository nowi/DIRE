package de.unima.dire.partitioning


import de.unima.dire.core.containers.ClauseStorage

/**
 * User: nowi
 * Date: 21.01.2010
 * Time: 19:47:10
 */

trait ClauseStoragePartitioning {
  def partition(clauses: ClauseStorage): List[ClauseStorage]
}