custom_palettes <- list(
  "cherry" = colorRampPalette(c("#A8E6CE", "#FFD3B5", "#FF8C94"))(10),
  "poncho" = colorRampPalette(c("#A7226E", "#F26B38", "#2F9599"))(10),
  "morning" = colorRampPalette(c("#E1F5C4", "#F9D423", "#FF4E50"))(10),
  "forest" = colorRampPalette(c("#FE4365", "#F9CDAD", "#83AF9B"))(10),
  "vice" = colorRampPalette(c("#3494e6", "#ec6ead"))(10),
  "dawn" = colorRampPalette(c("#f3904f", "#3b4371"))(10),
  "radar" = colorRampPalette(c("#a770ef", "#cf8bf3", "#fdb99b"))(10),
  "cosmic" = colorRampPalette(c("#ff00cc", "#333399"))(10),
  "nepal" = colorRampPalette(c("#de6161", "#2657eb"))(10),
  "azure" = colorRampPalette(c("#ef32d9", "#89fffd"))(10),
  "love" = colorRampPalette(c("#3a6186", "#89253e"))(10),
  "dehli" = colorRampPalette(c("#808080", "#3fada8"))(10),
  "sherbert" = colorRampPalette(c("#f79d00", "#64f38c"))(10),
  "dusk" = colorRampPalette(c("#2c3e50", "#fd746c"))(10),
  "grapefruit" = colorRampPalette(c("#e96443", "#904e95"))(10),
  "sunset" = colorRampPalette(c("#0b486b", "#f56217"))(10)
)

keys <- list(
  aeroway =
    c(
      "aerodrome", "apron", "gate", "hanger", "helipad", "heliport",
      "navigationaid", "runway", "spaceport", "taxilane", "taxiway",
      "terminal", "windsock"
    ),
  amenity =
    c(
      "bar", "bbq", "biergarten", "cafe", "drinking_water", "fast_food",
      "food_court", "ice_cream", "pub", "restaurant", "college",
      "kindergarten", "library", "archive", "public_bookcase", "school",
      "music_school", "driving_school", "language_school", "university",
      "research_institute", "bicycle_parking", "bicycle_repair_station",
      "bicycle_rental", "boat_rental", "boat_sharing", "buggy_parking",
      "bus_station", "car_rental", "car_sharing", "car_wash",
      "vehicle_inspection", "charging_station", "ferry_terminal", "fuel",
      "grit_bin", "motorcycle_parking", "parking", "parking_entrance",
      "parking_space", "taxi", "ticket_validator", "atm", "bank",
      "bureau_de_change", "baby_hatch", "clinic", "dentist", "doctors",
      "hospital", "nursing_home", "pharmacy", "social_facility", "veterinary",
      "blood_donation", "arts_centre", "brothel", "casino", "cinema",
      "community_centre", "fountain", "gambling", "music_venue", "nightclub",
      "planetarium", "social_centre", "stripclub", "studio", "swingerclub",
      "theatre", "animal_boarding", "animal_shelter", "baking_oven", "bench",
      "clock", "courthouse", "coworking_space", "crematorium", "crypt",
      "dive_centre", "dojo", "embassy", "fire_station", "game_feeding",
      "grave_yard", "hunting_stand", "internet_cafe", "kitchen",
      "marketplace", "photo_booth", "place_of_worship", "police", "post_box",
      "post_depot", "post_office", "prison", "public_bath", "ranger_station",
      "recycling", "resuce_station", "rv_storage", "sanitary_dump_station",
      "shelter", "shower", "table", "telephone", "toilets", "townhall",
      "vending_machine", "waste_basket", "waste_transfer_station",
      "watering_place", "water_point"
    ),
  building =
    c(
      "apartments", "farm", "hotel", "house", "detached", "residential",
      "dormitory", "terrace", "houseboat", "bungalow", "static_caravan",
      "cabin", "commercial", "office", "industrial", "retail", "supermarket",
      "warehouse", "kiosk", "cathedral", "chapel", "church", "mosque",
      "temple", "synagogue", "shrine", "bakehouse", "civic", "government",
      "hospital", "kindergarten", "school", "stadium", "train_station",
      "transportation", "university", "grandstand", "public", "toilets",
      "barn", "bridge", "bunker", "carport", "conservatory", "construction",
      "cowshed", "digester", "farm_auxiliary", "garage", "garages",
      "greenhouse", "hangar", "hut", "pavilion", "parking", "riding_hall",
      "roof", "shed", "sports_hall", "stable", "sty", "transformer_tower",
      "service", "ruins", "water_tower", "yes"
    ),
  craft =
    c(
      "bakery", "beekeeper", "blacksmith", "boatbuilder", "bookbinder",
      "brewery", "builder", "carpenter", "caterer", "clockmaker",
      "confectionery", "distillery", "dressmaker", "electronics_repair",
      "electrician", "floorer", "gardener", "handicraft", "hvac", "jeweller",
      "plyr::joiner", "key_cutter", "locksmith", "metal_construction",
      "optician", "painter", "photographer", "photographic_laboratory",
      "plasterer", "plumber", "pottery", "roofer", "saddler", "sawmill",
      "scaffolder", "sculptor", "shoemaker", "stonemason", "tailor", "tiler",
      "tinsmith", "upholsterer", "watchmaker", "window_construction", "winery"
    ),
  emergency =
    c(
      "ambulance_station", "defibrillator", "first_aid_kit", "landing_site",
      "emergency_ward_entrance", "dry_riser_inlet", "fire_alarm_box",
      "fire_extinguisher", "fire_hose", "fire_hydrant", "water_tank",
      "fire_water_pond", "suction_point", "lifeguard", "lifeguard_base",
      "lifeguard_tower", "lifeguard_platform", "life_ring", "mountain_rescue",
      "ses_station", "assembly_point", "access_point", "phone", "siren"
    ),
  historic =
    c(
      "aircraft", "archaeological_site", "battlefield", "boundary_stone",
      "buildig", "cannon", "castle", "church", "city_gate", "citywalls",
      "farm", "fort", "locomotive", "manor", "memorial", "milestone",
      "monastery", "monument", "pillory", "ruins", "rune_stone", "ship",
      "tank", "tomb", "tower", "wayside_cross", "wayside_shrine", "wreck",
      "yes"
    ),
  landuse =
    c(
      "commercial", "construction", "industrial", "residential", "retail",
      "allotments", "basin", "brownfield", "cemetry", "depot", "farmland",
      "farmyard", "forest", "garages", "greenfield",
      "greenhouse_horticulture", "landfill", "meadow", "military", "orchard",
      "plant_nursery", "port", "quarry", "railway", "recreation_ground",
      "religious", "salt_pond", "village_green", "vineyard"
    ),
  leisure =
    c(
      "adult_gaming_centre", "amusement_arcarde", "beach_resort", "bandstand",
      "bird_hide", "common", "dance", "disc_golf_course", "dog_park",
      "escape_game", "firepit", "fishing", "fitness_centre",
      "fitness_station", "garden", "hackerspace", "horse_riding", "ice_rink",
      "marina", "miniature_golf", "nature_reserve", "park", "picnic_table",
      "pitch", "playground", "slipway", "sports_centre", "stadium",
      "summer_camp", "swimming_area", "swimming_pool", "track", "water_park",
      "wildlife_hide"
    ),
  man_made =
    c(
      "adit", "beacon", "breakwater", "bridge", "bunker_silo", "campanile",
      "chimney", "communications_tower", "crane", "cross", "cutline",
      "clearcut", "dovecote", "dyke", "embankment", "flagpole", "gasometer",
      "groyne", "kiln", "lighthouse", "mast", "mineshaft",
      "monitoring_station", "obelisk", "observatory", "offshore_platform",
      "petroleum_well", "pier", "pipeline", "pumping_station",
      "reservoir_covered", "silo", "snow_fence", "storage_tank",
      "street_cabinet", "surveillance", "survey_point", "telescope", "tower",
      "wastewater_plant", "watermill", "water_tower", "water_well",
      "water_tap", "water_works", "windmill", "works", "yes"
    ),
  office =
    c(
      "accountant", "advertising_agency", "architect", "association",
      "charity", "company", "coworking", "diplomatic",
      "educational_institution", "employment_agency", "energy_supplier",
      "engineer", "estate_agent", "financial", "forestry", "foundation",
      "geodesist", "government", "guide", "insurance", "it", "lawyer",
      "logistics", "moving_company", "newspaper", "ngo", "notary", "parish",
      "political_party", "property_management", "publisher", "quango",
      "religion", "research", "surveyor", "tax", "tax_advisor",
      "telecommunication", "travel_agent", "water_utility", "yes"
    ),
  place =
    c(
      "country", "state", "region", "province", "district", "county",
      "municipality", "city", "borough", "suburb", "quarter", "neighbourhood",
      "city_block", "plot", "town", "village", "hamlet", "isolated_dewlling",
      "farm", "allotments", "continent", "archipelago", "island", "islet",
      "square", "locality", "sea", "ocean"
    ),
  public_transport =
    c(
      "stop_position", "platform", "station", "stop_area"
    ),
  railway =
    c(
      "abandoned", "construction", "disused", "funicular", "light_rail",
      "miniature", "monorail", "narrow_gauge", "preserved", "rail", "subway",
      "tram", "halt", "platform", "station", "subway_entrance", "tram_stop",
      "buffer_stop", "derail", "crossing", "level_crossing", "signal",
      "railway_crossing", "turntable", "roundhouse", "traverser", "wash"
    ),
  shop =
    c(
      "alcohol", "bakery", "beverages", "brewing_supplies", "butcher",
      "cheese", "chocolate", "coffee", "confectionery", "convenience", "deli",
      "dairy", "farm", "frozen_food", "greengrocer", "health_food",
      "ice_cream", "pasta", "pastry", "seafood", "spices", "street_vendor",
      "tea", "water", "department_store", "general", "kiosk", "mall",
      "supermarket", "wholesale", "baby_goods", "bag", "boutique", "clothese",
      "fabric", "fashion", "fashion_accessories", "jewelry", "leather",
      "sewing", "shoes", "tailor", "watches", "charity", "second_hand",
      "variety_store", "beauty", "chemist", "cosmetics", "erotic",
      "hairdresser", "hairdresser_supply", "hearing_aids", "herbalist",
      "massage", "medical_supply", "nutrition_supplements", "optician",
      "perfumery", "tattoo", "agrarian", "appliance", "bathroom_furnishing",
      "doityourself", "electrical", "energy", "fireplace", "florist",
      "garden_centre", "garden_furniture", "gas", "glaziery", "hardware",
      "houseware", "locksmith", "paint", "security", "trade", "antiques",
      "bed", "candles", "carpet", "curtain", "doors", "flooring", "furniture",
      "interior_decoration", "kitchen", "lamps", "tiles", "window_blind",
      "computer", "robot", "electronics", "hifi", "mobile_phone",
      "radiotechnics", "vacuum_cleaner", "atv", "bicycle", "boat", "car",
      "car_repair", "car_parts", "caravan", "fuel", "fishing", "free_flying",
      "hunting", "jetski", "motorcycle", "outdoor", "scuba_diving", "ski",
      "snowmobile", "sports", "swimming_pool", "trailer", "tyres", "art",
      "collector", "craft", "frame", "games", "model", "music",
      "musical_instrument", "photo", "camera", "trophy", "video",
      "video_games", "anime", "books", "gift", "lottery", "newsagent",
      "stationery", "ticket", "bookmaker", "cannabis", "copyshop",
      "dry_cleaning", "e-cigarette", "funeral_directors", "laundry",
      "money_lender", "party", "pawnbroker", "pet", "pet_grooming",
      "pyrotechnics", "religion", "storage_rental", "tobacco", "toys",
      "travel_agency", "vacant", "weapons", "pest_control"
    ),
  sport =
    c(
      "9pin", "10pin", "american_football", "archery", "athletics",
      "australian_football", "badminton", "baseball", "basketball",
      "billiards", "bmx", "boules", "bowls", "boxing", "canadian_football",
      "canoe", "chess", "climbing", "climbing_adventure", "cricket",
      "croquet", "curling", "cycling", "darts", "dog_racing", "equestrian",
      "field_hockety", "free_flying", "futsal", "gaelic_games", "golf",
      "gymnastics", "handball", "horseshoes", "horse_racing", "ice_hockey",
      "ice_skating", "ice_stock", "judo", "karate", "karting", "kitesurfing",
      "korfball", "lacrosse", "model_aerodrome", "motocross", "motor",
      "multi", "netball", "orienteering", "paddle_tennis", "padel",
      "paragliding", "pelota", "racquet", "rc_car", "rowing", "rugby_league",
      "rugby_union", "running", "sailing", "scuba_diving", "shooting",
      "skateboard", "soccer", "surfing", "swimming", "table_tennis",
      "table_soccer", "taekwondo", "tennis", "toboggan", "volleyball",
      "water_ski", "weightlifting", "yoga"
    ),
  tourism =
    c(
      "alpine_hut", "apartment", "aquarium", "artwork", "attraction",
      "camp_site", "caravan_site", "chalet", "gallery", "guest_house",
      "hostel", "hotel", "information", "motel", "museum", "picnic_site",
      "theme_park", "viewpoint", "wilderness_hut", "zoo", "yes"
    )
)

pairs <- list(
  "Education" = list(
    "key" = "amenity",
    "value" = c("college", "school", "university")
  ),
  "Entertainment" = list(
    "key" = "amenity",
    "value" = keys$amenity[56:70]
  ),
  "Healthcare" = list(
    "key" = "amenity",
    "value" = keys$amenity[46:55]
  ),
  "Housing" = list(
    "key" = "building",
    "value" = keys$building[1:12]
  ),
  "Leisure" = list(
    "key" = "leisure",
    "value" = NULL
  ),
  "Office buildings" = list(
    "key" = "office",
    "value" = NULL
  ),
  "Public transport" = list(
    "key" = "public_transport",
    "value" = NULL
  ),
  "Restaurants, bars and food shops" = list(
    "key" = "amenity",
    "value" = keys$amenity[1:10]
  )
)