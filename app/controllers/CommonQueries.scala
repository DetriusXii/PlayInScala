package controllers

import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas._
import com.squeryl.jdip.tables._

object CommonQueries {
	private def getFormattedLocationName(location: Location): String = location match {
    	case Location(prov, Coast.NO_COAST) => prov
    	case Location(prov, Coast.ALL_COAST) => prov
    	case Location(prov, coast) => "%s-%s" format (prov, coast)
	}
  
	def getMoves(srcLocationID: Int): Iterable[Location] = from(Jdip.locations)(loc => 
              where(loc.id in from(Jdip.adjacencies)(adj =>
                where(adj.srcLocation === srcLocationID) select(adj.dstLocation)))
              select(loc))
    
    def getMoves(srcDiplomacyUnit: DiplomacyUnit): Iterable[Location] = getMoves(srcDiplomacyUnit.unitLocation)
    
    def getFormattedMoves(srcLocationID: Int): Iterable[String] = 
      getMoves(srcLocationID).map(getFormattedLocationName(_))
      
    def getMovesOrdersMap(diplomacyUnits: Iterable[DiplomacyUnit]) = 
      diplomacyUnits.map(dpu => {
                val srcLocationOption = Jdip.locations.lookup(dpu.unitLocation)
                srcLocationOption.map(loc => (getFormattedLocationName(loc), 
                    getFormattedMoves(dpu.unitLocation)))
              }).flatten
              
    def getSupportHolds(srcLocationID: Int): Iterable[Location] = {
            val srcAdjacencies = Jdip.adjacencies.filter(adj => adj.srcLocation == srcLocationID)
            val dpuProvinces = 
              Jdip.diplomacyUnits.map(dpu => Jdip.locations.lookup(dpu.unitLocation)).flatten.map(_ match {
                case Location(province, _) => province
              })
            val srcAdjacenciesWithUnitPresent = srcAdjacencies.filter(adj => {
              val dstLocationOption = Jdip.locations.lookup(adj.dstLocation)
              dstLocationOption match {
                case Some(Location(province, _)) => dpuProvinces.exists(_ == province)
                case None => false
              }
            })
            val dstLocationsWithUnitPresent = 
              srcAdjacenciesWithUnitPresent.map(adj => Jdip.locations.lookup(adj.dstLocation)).flatten
            dstLocationsWithUnitPresent
          }
	
	 def getSupportHolds(srcDiplomacyUnit: DiplomacyUnit): Iterable[Location] = 
	   getSupportHolds(srcDiplomacyUnit.unitLocation)
	
	 def getFormattedSupportHolds(srcLocationID: Int): Iterable[String] =
	   getSupportHolds(srcLocationID).map(getFormattedLocationName(_))
	  
	 def getSupportHoldsMap(diplomacyUnits: Iterable[DiplomacyUnit]) =
	   diplomacyUnits.map(dpu => {
                val srcLocationOption = Jdip.locations.lookup(dpu.unitLocation)
                srcLocationOption.map(loc => 
                  (getFormattedLocationName(loc), getSupportHolds(dpu.unitLocation).map(getFormattedLocationName(_))))
       }).flatten
	
	 def getSupportMoves(srcLocationID: Int): Iterable[(Location, Iterable[Location])] = {
	   val movesForAllOtherUnits = 
	     Jdip.diplomacyUnits.filter(_.unitLocation != srcLocationID).map(dpu =>
	      	(dpu, getMoves(dpu.unitLocation))
	     )
	   val movesForThisUnit = getMoves(srcLocationID)
	   
	   val movesThatMatterFromOtherUnits = movesForAllOtherUnits.map(u => {
	     val otherUnit = u._1
	     val movesForOtherUnit = u._2
	     
	     val movesThatMatterFromOtherUnit = movesForOtherUnit.filter(_ match {
	       case Location(dstProvinceForOtherUnit, _) => movesForThisUnit.exists(_ match {
	         case Location(`dstProvinceForOtherUnit`, _) => true
	         case Location(_, _) => false
	       })
	     })
	     
	     Jdip.locations.lookup(otherUnit.unitLocation).map((_, movesThatMatterFromOtherUnit))
	   }).flatten
	   
	   movesThatMatterFromOtherUnits.filter(_ match {
	     case (_, movesFromOtherUnit) => !movesFromOtherUnit.isEmpty
	   })
     }
	 
	 def getSupportMoves(srcDiplomacyUnit: DiplomacyUnit): Iterable[(Location, Iterable[Location])] =
	   getSupportMoves(srcDiplomacyUnit.unitLocation)
	 
	 def getFormattedSupportMoves(srcLocationID: Int): Iterable[(String, Iterable[String])] =
	   getSupportMoves(srcLocationID).map(u => (getFormattedLocationName(u._1), u._2.map(getFormattedLocationName(_))))

	 def getMovesByConvoy(srcDiplomacyUnit: DiplomacyUnit): Iterable[(Location, Iterable[Location]) = {
	   (srcDiplomacyUnit.unitType match {
	     case UnitType.ARMY => Some(srcDiplomacyUnit)
	     case UnitType.FLEET => None
	   }).flatMap(dpu => {
	     val unitLocationOption: Option[Location] = Jdip.locations.lookup(dpu.unitLocation)
	     
	     val coastsOnProvinceOption: Option[Iterable[Location]] = unitLocationOption.map(_ match {
	       case Location(province, Coast.NO_COAST) => Jdip.locations.filter(_ match {
	         case Location(`province`, coast) => !coast.equals(Coast.NO_COAST)
	       })
	     })
	     
	     coastsOnProvinceOption.map((dpu, _))
	   }).flatMap((u: Tuple2[DiplomacyUnit, Iterable[Location]]) => {
	     
	   })
	 }
}