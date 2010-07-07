package de.unima.dire.core.containers

/**
 * User: nowi
 * Date: 20.05.2010
 * Time: 09:48:50
 */

import de.unima.dire.core.index.FeatureVectorImperfectIndex

class LightestClauseHeuristicWithTopSymbolHashingIndexSpec extends UsableClausesStoreSpec {
  override def createStorage = new MutableClauseStore with LightestClauseHeuristicStorage with FeatureVectorImperfectIndex
}