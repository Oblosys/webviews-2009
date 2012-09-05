module Imported where

import DatabaseTypes

lenders = [ Lender {lenderId = LenderId {lenderIdLogin = "martijn"}, _lenderFirstName = "Martijn", lenderLastName = "Schrage", lenderGender = M, lenderMail = "martijn@oblomov.com", lenderStreet = "Kerkstraat", lenderStreetNr = "15", lenderCity = "Utrecht", _lenderZipCode = "3581 RA", lenderCoords = (5.130862,52.0927779), lenderImage = "lender_1.jpg", lenderRating = 5, lenderNrOfPoints = 18, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Henny"}, _lenderFirstName = "Henny", lenderLastName = "Verweij", lenderGender = M, lenderMail = "Henny@gmail.com", lenderStreet = "Franz Schubertstraat", lenderStreetNr = "39", lenderCity = "Utrecht", _lenderZipCode = "3533 GT", lenderCoords = (5.0873201,52.086051), lenderImage = "lender_2.jpg", lenderRating = 5, lenderNrOfPoints = 312, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Jaap"}, _lenderFirstName = "Jaap", lenderLastName = "Lageman", lenderGender = M, lenderMail = "jaap@bpcutrecht.nl", lenderStreet = "Kerkstraat", lenderStreetNr = "17", lenderCity = "Utrecht", _lenderZipCode = "3581 RA", lenderCoords = (5.1309057,52.092796), lenderImage = "lender_3.jpg", lenderRating = 5, lenderNrOfPoints = 238, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Hans"}, _lenderFirstName = "Hans", lenderLastName = "Pietersen", lenderGender = M, lenderMail = "Hans@Gmail.com", lenderStreet = "traay", lenderStreetNr = "18", lenderCity = "Driebergen", _lenderZipCode = "", lenderCoords = (5.2842897,52.0524575), lenderImage = "lender_4.jpg", lenderRating = 0, lenderNrOfPoints = 32, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Frans"}, _lenderFirstName = "Frans", lenderLastName = "Verbeek", lenderGender = M, lenderMail = "Frans@Gmail.com", lenderStreet = "slotlaan", lenderStreetNr = "19", lenderCity = "Zeist", _lenderZipCode = "", lenderCoords = (5.2400272,52.0827018), lenderImage = "lender_5.jpg", lenderRating = 0, lenderNrOfPoints = 12, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Frits"}, _lenderFirstName = "Frits", lenderLastName = "Spits", lenderGender = M, lenderMail = "Frits@gmail.com", lenderStreet = "", lenderStreetNr = "", lenderCity = "Zeist", _lenderZipCode = "", lenderCoords = (5.2332526,52.0906015), lenderImage = "lender_6.jpg", lenderRating = 0, lenderNrOfPoints = 10, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Piet"}, _lenderFirstName = "Piet", lenderLastName = "Paaltjes", lenderGender = M, lenderMail = "Piet@gmail.com", lenderStreet = "", lenderStreetNr = "", lenderCity = "Zeist", _lenderZipCode = "", lenderCoords = (5.2332526,52.0906015), lenderImage = "lender_7.jpg", lenderRating = 0, lenderNrOfPoints = 2, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Xander"}, _lenderFirstName = "Xander", lenderLastName = "de Bouvier", lenderGender = M, lenderMail = "Xander@gmail.com", lenderStreet = "", lenderStreetNr = "", lenderCity = "Zeist", _lenderZipCode = "", lenderCoords = (5.2332526,52.0906015), lenderImage = "lender_8.jpg", lenderRating = 0, lenderNrOfPoints = 0, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Truus"}, _lenderFirstName = "Truus", lenderLastName = "Hanekam", lenderGender = F, lenderMail = "Truus@gmail.com", lenderStreet = "", lenderStreetNr = "", lenderCity = "Amsterdam", _lenderZipCode = "", lenderCoords = (4.8951679,52.3702157), lenderImage = "lender_9.jpg", lenderRating = 0, lenderNrOfPoints = 44, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Anita"}, _lenderFirstName = "Anita", lenderLastName = "Burgemeester", lenderGender = F, lenderMail = "Anita@gmail.com", lenderStreet = "", lenderStreetNr = "", lenderCity = "Amsterdam", _lenderZipCode = "", lenderCoords = (4.8951679,52.3702157), lenderImage = "lender_10.jpg", lenderRating = 0, lenderNrOfPoints = 12, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Hanneke"}, _lenderFirstName = "Hanneke", lenderLastName = "Rock", lenderGender = F, lenderMail = "Hanneke@gmail.com", lenderStreet = "", lenderStreetNr = "", lenderCity = "Amsterdam", _lenderZipCode = "", lenderCoords = (4.8951679,52.3702157), lenderImage = "lender_11.jpg", lenderRating = 0, lenderNrOfPoints = 93, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Irene"}, _lenderFirstName = "Irene", lenderLastName = "Rood", lenderGender = F, lenderMail = "Irene@gmail.com", lenderStreet = "", lenderStreetNr = "", lenderCity = "Amsterdam", _lenderZipCode = "", lenderCoords = (0.0,0.0), lenderImage = "lender_12.jpg", lenderRating = 0, lenderNrOfPoints = 174, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Carla"}, _lenderFirstName = "Carla", lenderLastName = "Baars", lenderGender = F, lenderMail = "Carla@gmail.com", lenderStreet = "", lenderStreetNr = "", lenderCity = "Hilversum", _lenderZipCode = "", lenderCoords = (0.0,0.0), lenderImage = "lender_13.jpg", lenderRating = 0, lenderNrOfPoints = 23, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Karin"}, _lenderFirstName = "Karin", lenderLastName = "Snoek", lenderGender = F, lenderMail = "Karin@gmail.com", lenderStreet = "", lenderStreetNr = "", lenderCity = "Hilversum", _lenderZipCode = "", lenderCoords = (0.0,0.0), lenderImage = "lender_14.jpg", lenderRating = 0, lenderNrOfPoints = 8, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Jessie"}, _lenderFirstName = "Jessie", lenderLastName = "Smid", lenderGender = F, lenderMail = "Jessie@gmail.com", lenderStreet = "", lenderStreetNr = "", lenderCity = "Hilversum", _lenderZipCode = "", lenderCoords = (0.0,0.0), lenderImage = "lender_15.jpg", lenderRating = 0, lenderNrOfPoints = 12, lenderItems = []}
         , Lender {lenderId = LenderId {lenderIdLogin = "Matt"}, _lenderFirstName = "Matt", lenderLastName = "Jansen", lenderGender = M, lenderMail = "Matt@gmail.com", lenderStreet = "kennedylaan", lenderStreetNr = "17", lenderCity = "Utrecht", _lenderZipCode = "", lenderCoords = (0.0,0.0), lenderImage = "lender_16.jpg", lenderRating = 0, lenderNrOfPoints = 15, lenderItems = []}
         ]
items = [ Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "martijn"}, itemPrice = 0, itemName = "Abbey Road", itemDescr = "Come Together\nSomething\nMaxwell's Silver Hammer\nOh! Darling\nOctopus's Garden\nI Want You (she's So Heavy)\nHere Comes The Sun\nBecause\nYou Never Give Me Your Money\nSun King\nMean Mr. Mustard\nPolythene Pam\nShe Came In Through The Bathroom Window\nGolden Slumbers\nCarry That Weight\nThe End\nHer Majesty", itemState = "Prima", itemImage = "cd_1.jpg", itemCategory = CD {cdArtist = "The Beatles", cdYear = 1969, cdGenre = "Pop/Rock"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Henny"}, itemPrice = 0, itemName = "Mahler Symphonies", itemDescr = "Disk 1\n1. New York Philharmonic - I. Langsam. Schleppend. Wie Ein Naturlaut. Im Anfang Sehr Gem\65533chlich - \n2. Ii. Kr\65533ftig Bewegt, Doch Nicht Zu Schnell - 2008 Remastered\n3. Iii. Feierlich Und Gemessen, Ohne Zu Schleppen - 2008 Remastered\n4. Iv. St\65533rmisch Bewegt - Energisch - 2008 Remastered\nDisk 2\n1. New York Philharmonic - Symphony No. 2 In C Minor 'resurrection': I. Allegro Maestoso - 2008 R\nDisk 3\n1. New York Philharmonic / Collegiate Chorale - Ii. Andante Moderato - 2008 Remastered\n2. Iii. In Ruhig Fliessender Bewegung - 2008 Remastered\n3. Iv. 'urlicht'. Sehr Feierlich, Aber Schlicht - 2008 Remastered\n4. V. Im Tempo Des Scherzos. Wild Herausfahrend - 2008 Remastered\nDisk 4\n1. New York Philharmonic - Symphony No. 3 In D Minor: I. Kr\65533ftig - 2008 Remastered\nDisk 5\n1. New York Philharmonic / Women's Chorus of the Schola Cantorum / Boys' - Ii. Tempo Di Menuetto - 2002. Iii. Comodo. Scherzando - 2008 Remastered\n3. Iv. Sehr Langsam\n4. V. Lustig Im Tempo Und Keck Im Ausdruck - 2008 Remastered\n5. Vi. Langsam - 2008 Remastered", itemState = "", itemImage = "cd_2.jpg", itemCategory = CD {cdArtist = "Mahler", cdYear = 2008, cdGenre = "Klassiek"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Matt"}, itemPrice = 0, itemName = "John Cage", itemDescr = "", itemState = "", itemImage = "cd_3.jpg", itemCategory = CD {cdArtist = "John Cage", cdYear = 0, cdGenre = ""}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Hans"}, itemPrice = 0, itemName = "Guitar Transcriptions", itemDescr = "1. Bach: Partita for Flute solo in A minor, BWV 1013\n2. Bach: Well-Tempered Clavier, Book 1: Prelude and Fugue no 9 in E major, BWV 854\n3. Bach: Capriccio in B flat major on the departure of his Most Beloved Brother, BWV 992\n4. Bach: Concerto in D minor after Alessandro Marcello, BWV 974 \n5. Bach: Sonata for Violin solo no 1 in G minor, BWV 1001 \n6. 5 Little Preludes, Bwv 939-943: Prelude In C Major, Bwv 939 \n7. Capriccio Sopra La Lontananza De Il Fratro Dilettissimo In B Flat Majo \n8. Keyboard Concerto In D Minor, Bwv 974 (After A. Marcello's Oboe Concert\n9. Keyboard Concerto In D Minor, Bwv 974 (After A. Marcello's Oboe Concert\n10. Keyboard Concerto In D Minor, Bwv 974 (After A. Marcello's Oboe Concert\n11. Violin Sonata No. 1 In G Minor, Bwv 1001 (Arr. E. Voorhorst for Guitar)\n12. Violin Sonata No. 1 In G Minor, Bwv 1001 (Arr. E. Voorhorst for Guitar)\n13. Violin Sonata No. 1 In G Minor, Bwv 1001 (Arr. E. Voorhorst for Guitar)\n14. Violin Sonata No. 1 In G Minor, Bwv 1001 (Arr. E. Voorhorst for Guitar", itemState = "", itemImage = "cd_4.jpg", itemCategory = CD {cdArtist = "Bach", cdYear = 2005, cdGenre = "Klassiek"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Frans"}, itemPrice = 0, itemName = "Violin Concertos BWV 1041/1042/1043/1060l", itemDescr = "1. Bach: Concerto for Violin no 1 in A minor, BWV 1041 \n2. Bach: Concerto for Violin no 2 in E major, BWV 1042 \n3. Bach: Concerto for 2 Violins in D minor, BWV 1043 \n4. Bach: Concerto for Oboe and Violin in C minor, BWV 1060", itemState = "", itemImage = "cd_5.jpg", itemCategory = CD {cdArtist = "Bach", cdYear = 2004, cdGenre = "Klassiek"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Frits"}, itemPrice = 0, itemName = "Slash", itemDescr = "1. Apocalyptic Love\n2. One Last Thrill\n3. Standing In the Sun\n4. You're A Lie\n5. No More Heroes\n6. Halo\n7. We Will Roam\n8. Anastasia\n9. Not for Me\n10. Bad Rain\n 11. Hard & Fast\n12. Far and Away\n 13. Shots Fired\n14. Carolina (Bonustrack)\n15. Crazy Life (Bonustrack)\nDisk 2  1. Apocalyptic Love Documentary ", itemState = "", itemImage = "cd_6.jpg", itemCategory = CD {cdArtist = "Slash", cdYear = 2012, cdGenre = "Heavy Metal"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Piet"}, itemPrice = 0, itemName = "Eye in the sky", itemDescr = "1. Sirius (1:57)\n2. Eye In The Sky (4:35)\n3. Children Of The Moon (4:49)\n4. Gemini (2:09)\n5. Silence And I (7:17)\n6. You're Gonna Get Your Fingers Burned (4:19)\n7. Psychobabble (4:50)\n8. Mammagamma - Instrumental (3:34)\n9. Step By Step (3:52)\n10. Old And Wise (4:57)\n11. Sirius - Demo (1:53)\n12. Old & Wise - Eric Woolfson Guide Vocal (4:31)\n13. Any Other Day - Studio Demo (1:40\n\n14. Silence & I - Early Version; Eric Woolfson Guide Vocal (7:33)\n15. The Naked Eye (10:47)\n16. Eye Pieces - Classical Naked Eye (7:51) ", itemState = "", itemImage = "cd_7.jpg", itemCategory = CD {cdArtist = "Alan Parsons Project", cdYear = 1980, cdGenre = "Pop/Rock"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Xander"}, itemPrice = 0, itemName = "Dark Side of the Moon", itemDescr = "1. Speak To Me (2011 - Remaster)\n2. Breathe (In the Air) [2011 - Remaster]\n3. On the Run (2011 - Remaster)\n4. Time (2011 - Remaster)\n5. The Great Gig In the Sky (2011 - Remaster)\n6. Money (2011 - Remaster)\n7. Us and Them (2011 - Remaster)\n8. Any Colour You Like (2011 - Remaster)\n9. Brain Damage (2011 - Remaster)\n10. Eclipse (2011 - Remaster)", itemState = "", itemImage = "cd_8.jpg", itemCategory = CD {cdArtist = "Pink Floyd", cdYear = 1973, cdGenre = "Pop/Rock"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Truus"}, itemPrice = 0, itemName = "The Koln Concert", itemDescr = "1. Part I\n2. Part IIa\n3. Part IIb\n4. Part IIc ", itemState = "", itemImage = "cd_9.jpg", itemCategory = CD {cdArtist = "Keith Jarret", cdYear = 1994, cdGenre = "Jazz"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Anita"}, itemPrice = 0, itemName = "Khmer", itemDescr = "1. Khmer\n2. Tlon\n3. Access / Song Of Sand I\n4. On Stream\n5. Platonic Years\n6. Phum\n7. Song Of Sand Ii\n8. Exit", itemState = "", itemImage = "cd_10.jpg", itemCategory = CD {cdArtist = "Nils Petter Molvaer", cdYear = 2005, cdGenre = "Jazz"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Henny"}, itemPrice = 0, itemName = "13 uur", itemDescr = "", itemState = "", itemImage = "book_2.jpg", itemCategory = Book {bookAuthor = "Deon Meyer", bookYear = 0, bookLanguage = "Nederlands", bookGenre = "Roman", bookPages = 0, bookISBN = ""}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Matt"}, itemPrice = 0, itemName = "Chantage", itemDescr = "", itemState = "", itemImage = "book_3.jpg", itemCategory = Book {bookAuthor = "Peter R_mer", bookYear = 0, bookLanguage = "Nederlands", bookGenre = "Roman", bookPages = 0, bookISBN = ""}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Hans"}, itemPrice = 0, itemName = "Genadeloos", itemDescr = "", itemState = "", itemImage = "book_4.jpg", itemCategory = Book {bookAuthor = "Karin Slaughter", bookYear = 0, bookLanguage = "Nederlands", bookGenre = "Roman", bookPages = 0, bookISBN = ""}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Frans"}, itemPrice = 0, itemName = "Hitchhikers Guide to the calaxy", itemDescr = "Seconds before the Earth is demolished to make way for a galactic freeway, Arthur Dent is plucked off the planet by his friend Ford Prefect, a researcher for the revised edition of The Hitchhiker's Guide to the Galaxy who, for the last fifteen years, has been posing as an out-of-work actor.\nTogether this dynamic pair begin a journey through space aided by quotes from The Hitchhiker's Guide (\"A towel is about the most massively useful thing an interstellar hitchhiker can have\") and a galaxy-full of weirdos. ", itemState = "", itemImage = "book_5.jpg", itemCategory = Book {bookAuthor = "Douglas Adams", bookYear = 1979, bookLanguage = "Engels", bookGenre = "SiFi", bookPages = 244, bookISBN = "9,78033E+12"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Frits"}, itemPrice = 0, itemName = "The cider house rules", itemDescr = "", itemState = "", itemImage = "book_6.jpg", itemCategory = Book {bookAuthor = "John Irving", bookYear = 0, bookLanguage = "Engels", bookGenre = "roman", bookPages = 0, bookISBN = ""}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Piet"}, itemPrice = 0, itemName = "Riverworld: Including To Your Scattered Bodies Go & The Fabulous Riverboat", itemDescr = "Charts a territory somewhere between \"Gulliver's Travels\" and \"The Lord of the Rings\".\n To Your Scattered Bodies Go\" and \"The Fabulous Riverboat \n Combined for the first time in one volume! \n Imagine that every human who ever lived, from the earliest Neanderthals to the present, is resurrected after death on the banks of an astonishing and seemingly endless river on an unknown world. They are miraculously provided with food, but with not a clue to the possible meaning of this strange afterlife. And so billions of people from history, and before, must start living again. \n Some set sail on the great river questing for the meaning of their resurrection, and to find and confront their mysterious benefactors. On this long journey, we meet Sir Richard Francis Burton, Mark Twain, Odysseus, Cyrano de Bergerac, and many others, most of whom embark upon searches of their own in this huge afterlife.", itemState = "", itemImage = "book_7.jpg", itemCategory = Book {bookAuthor = "Philip Jose Farmer", bookYear = 0, bookLanguage = "Engels", bookGenre = "sifi", bookPages = 443, bookISBN = "9,78077E+12"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Xander"}, itemPrice = 0, itemName = "The Red House", itemDescr = "", itemState = "", itemImage = "book_8.jpg", itemCategory = Book {bookAuthor = "Mark Haddon", bookYear = 0, bookLanguage = "Engels", bookGenre = "roman", bookPages = 0, bookISBN = ""}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Truus"}, itemPrice = 0, itemName = "Waar mensen gaan wordt tijd geschreven", itemDescr = "", itemState = "", itemImage = "book_9.jpg", itemCategory = Book {bookAuthor = "Theo Ettema", bookYear = 0, bookLanguage = "Nederlands", bookGenre = "gedichten", bookPages = 0, bookISBN = ""}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Anita"}, itemPrice = 0, itemName = "Da Vinci Code", itemDescr = "", itemState = "", itemImage = "book_10.jpg", itemCategory = Book {bookAuthor = "Dan Brown", bookYear = 0, bookLanguage = "Nederlands", bookGenre = "thriller", bookPages = 0, bookISBN = ""}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Hanneke"}, itemPrice = 0, itemName = "De laatste oorlog", itemDescr = "", itemState = "", itemImage = "book_11.jpg", itemCategory = Book {bookAuthor = "Jan Marijnissen en Karel Glasstra van Loon", bookYear = 0, bookLanguage = "Nederlands", bookGenre = "filosofie", bookPages = 0, bookISBN = ""}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Irene"}, itemPrice = 0, itemName = "Fred en Wilma", itemDescr = "", itemState = "", itemImage = "book_12.jpg", itemCategory = Book {bookAuthor = "Nieke Oosterbaan", bookYear = 0, bookLanguage = "Nederlands", bookGenre = "doe het zelf", bookPages = 125, bookISBN = ""}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Carla"}, itemPrice = 0, itemName = "Oeroeg", itemDescr = "", itemState = "", itemImage = "book_13.jpg", itemCategory = Book {bookAuthor = "Hella Haasen", bookYear = 1948, bookLanguage = "Nederlands", bookGenre = "boekenweekgeschenk", bookPages = 80, bookISBN = ""}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Karin"}, itemPrice = 0, itemName = "Hollandse Polders", itemDescr = "", itemState = "", itemImage = "book_14.jpg", itemCategory = Book {bookAuthor = "Willem van der Ham", bookYear = 1995, bookLanguage = "Nederlands", bookGenre = "geschiedenis", bookPages = 230, bookISBN = ""}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "martijn"}, itemPrice = 0, itemName = "Moon", itemDescr = "Astronaut Sam Bell woont al bijna drie jaar op de maan in opdracht van Lunar Industries. Hij leeft daar een eenzaam bestaan en door een defecte satelliet heeft hij amper contact met zijn vrouw Tess en driejarige dochter Eve. Zijn enige maatje aan boord is Gerty: een robot. Onverwacht krijgt Sam gezondheidsproblemen en gaat hallucineren waardoor hij een bijna-fataal ongeluk krijgt. Als hij terug is op zijn basisstation, ontmoet hij zijn jongere en agressieve \"ik\". ", itemState = "Goed", itemImage = "dvd_1.jpg", itemCategory = DVD {dvdMovieOrSeries = Series, dvdDirector = "Duncan Jones", dvdLanguage = "Engels", dvdYear = 2009, dvdGenre = "Science Fiction", dvdRunningTime = 97, dvdIMDb = "http://www.imdb.com/title/tt1182345/", dvdSeason = 0, dvdNrOfEpisodes = 0}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "martijn"}, itemPrice = 0, itemName = "Deadwood season 1", itemDescr = "Deadwood is een televisieserie die zich aan het einde van de negentiende eeuw afspeelt in Deadwood (South Dakota), een stadje dat toentertijd beheerst werd door de goudkoorts. Na drie seizoenen is de serie gestopt. Wel zijn er plannen om \65533\65533n of meerdere films als vervolg van de serie te maken.", itemState = "Goed", itemImage = "dvd_2.jpg", itemCategory = DVD {dvdMovieOrSeries = Series, dvdDirector = "-", dvdLanguage = "Engels", dvdYear = 2004, dvdGenre = "Western", dvdRunningTime = 45, dvdIMDb = "http://www.imdb.com/title/tt0348914/", dvdSeason = 1, dvdNrOfEpisodes = 12}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Henny"}, itemPrice = 0, itemName = "Nikitia", itemDescr = "", itemState = "", itemImage = "dvd_3.jpg", itemCategory = DVD {dvdMovieOrSeries = Series, dvdDirector = "", dvdLanguage = "", dvdYear = 2005, dvdGenre = "", dvdRunningTime = 0, dvdIMDb = "", dvdSeason = 0, dvdNrOfEpisodes = 0}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Matt"}, itemPrice = 0, itemName = "Bloedverwanten", itemDescr = "", itemState = "", itemImage = "dvd_4.jpg", itemCategory = DVD {dvdMovieOrSeries = Series, dvdDirector = "", dvdLanguage = "", dvdYear = 2006, dvdGenre = "", dvdRunningTime = 0, dvdIMDb = "", dvdSeason = 0, dvdNrOfEpisodes = 0}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Hans"}, itemPrice = 0, itemName = "Harry Potter, Steen der Wijzen", itemDescr = "", itemState = "", itemImage = "dvd_5.jpg", itemCategory = DVD {dvdMovieOrSeries = Series, dvdDirector = "", dvdLanguage = "", dvdYear = 2007, dvdGenre = "", dvdRunningTime = 0, dvdIMDb = "", dvdSeason = 0, dvdNrOfEpisodes = 0}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Frans"}, itemPrice = 0, itemName = "Ghost Protocol", itemDescr = "", itemState = "", itemImage = "dvd_6.jpg", itemCategory = DVD {dvdMovieOrSeries = Series, dvdDirector = "", dvdLanguage = "", dvdYear = 2008, dvdGenre = "", dvdRunningTime = 0, dvdIMDb = "", dvdSeason = 0, dvdNrOfEpisodes = 0}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Frits"}, itemPrice = 0, itemName = "Pirates of the Caribbean", itemDescr = "", itemState = "", itemImage = "dvd_7.jpg", itemCategory = DVD {dvdMovieOrSeries = Series, dvdDirector = "", dvdLanguage = "", dvdYear = 2009, dvdGenre = "", dvdRunningTime = 0, dvdIMDb = "", dvdSeason = 0, dvdNrOfEpisodes = 0}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Piet"}, itemPrice = 0, itemName = "Sherlock Holmes", itemDescr = "", itemState = "", itemImage = "dvd_8.jpg", itemCategory = DVD {dvdMovieOrSeries = Series, dvdDirector = "", dvdLanguage = "", dvdYear = 2010, dvdGenre = "", dvdRunningTime = 0, dvdIMDb = "", dvdSeason = 0, dvdNrOfEpisodes = 0}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Xander"}, itemPrice = 0, itemName = "The Girl with the Dragon Tattoo", itemDescr = "", itemState = "", itemImage = "dvd_9.jpg", itemCategory = DVD {dvdMovieOrSeries = Series, dvdDirector = "", dvdLanguage = "", dvdYear = 2011, dvdGenre = "", dvdRunningTime = 0, dvdIMDb = "", dvdSeason = 0, dvdNrOfEpisodes = 0}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Truus"}, itemPrice = 0, itemName = "Deadwood season 2", itemDescr = "", itemState = "", itemImage = "dvd_10.jpg", itemCategory = DVD {dvdMovieOrSeries = Series, dvdDirector = "-", dvdLanguage = "Engels", dvdYear = 2005, dvdGenre = "Western", dvdRunningTime = 45, dvdIMDb = "http://www.imdb.com/title/tt0348914/", dvdSeason = 0, dvdNrOfEpisodes = 0}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Anita"}, itemPrice = 0, itemName = "Elvis", itemDescr = "", itemState = "", itemImage = "dvd_11.jpg", itemCategory = DVD {dvdMovieOrSeries = Series, dvdDirector = "", dvdLanguage = "Engels", dvdYear = 2006, dvdGenre = "muziek", dvdRunningTime = 0, dvdIMDb = "", dvdSeason = 0, dvdNrOfEpisodes = 0}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Hanneke"}, itemPrice = 0, itemName = "Gossip Girl", itemDescr = "", itemState = "", itemImage = "dvd_12.jpg", itemCategory = DVD {dvdMovieOrSeries = Series, dvdDirector = "", dvdLanguage = "Engels", dvdYear = 2005, dvdGenre = "soap", dvdRunningTime = 0, dvdIMDb = "", dvdSeason = 0, dvdNrOfEpisodes = 0}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Irene"}, itemPrice = 0, itemName = "The Box", itemDescr = "", itemState = "", itemImage = "dvd_13.jpg", itemCategory = DVD {dvdMovieOrSeries = Series, dvdDirector = "", dvdLanguage = "Engels", dvdYear = 2004, dvdGenre = "Thriller", dvdRunningTime = 0, dvdIMDb = "", dvdSeason = 0, dvdNrOfEpisodes = 0}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Carla"}, itemPrice = 0, itemName = "Lost", itemDescr = "", itemState = "", itemImage = "dvd_14.jpg", itemCategory = DVD {dvdMovieOrSeries = Series, dvdDirector = "", dvdLanguage = "Engels", dvdYear = 2011, dvdGenre = "Suspense", dvdRunningTime = 0, dvdIMDb = "", dvdSeason = 0, dvdNrOfEpisodes = 0}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "martijn"}, itemPrice = 0, itemName = "Grand Theft Auto V", itemDescr = "Wat is er tegenwoordig nog over van die legendarische 'American Dream'? Niko Bellic is net aan wal gestapt na een lange bootreis uit Europa en hoopt in Amerika zijn verleden te begraven. Zijn neef Roman droomt ervan het helemaal te maken in Liberty City, in het land van de onbegrensde mogelijkheden.\nZe raken in de schulden en komen in het criminele circuit terecht door toedoen van oplichters, dieven en ander tuig. Langzamerhand komen ze erachter dat ze hun dromen niet kunnen waarmaken in een stad waar alles draait om geld en status. Heb je genoeg geld, dan staan alle deuren voor je open. Zonder een cent beland je in de goot.", itemState = "Goed", itemImage = "game_1.jpg", itemCategory = Game {gamePlatform = "PlayStation 3", gameYear = 2011, gameDeveloper = "Rockstar Games", gameGenre = "Action adventure, Open world"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Henny"}, itemPrice = 0, itemName = "Pokemon Black 2", itemDescr = "", itemState = "matig", itemImage = "game_2.jpg", itemCategory = Game {gamePlatform = "Nindento DS", gameYear = 2011, gameDeveloper = "Pokemon Games", gameGenre = "Action adventure"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Matt"}, itemPrice = 0, itemName = "MIB", itemDescr = "", itemState = "goed", itemImage = "game_3.jpg", itemCategory = Game {gamePlatform = "Wii", gameYear = 2011, gameDeveloper = "SIFI Games", gameGenre = "Action adventure"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Hans"}, itemPrice = 0, itemName = "FiFa12", itemDescr = "", itemState = "kapotgespeelt", itemImage = "game_4.jpg", itemCategory = Game {gamePlatform = "Wii", gameYear = 2011, gameDeveloper = "voetbal Games", gameGenre = "Sport, voetbal"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Frans"}, itemPrice = 0, itemName = "Spec Ops The Line", itemDescr = "", itemState = "slecht", itemImage = "game_5.jpg", itemCategory = Game {gamePlatform = "PS3", gameYear = 2011, gameDeveloper = "vecht en CO", gameGenre = "Oorlog, geweld, schieten, dood!"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Frits"}, itemPrice = 0, itemName = "K3 fashion", itemDescr = "", itemState = "matig", itemImage = "game_6.jpg", itemCategory = Game {gamePlatform = "Nindento DS", gameYear = 2011, gameDeveloper = "Roze en blauw", gameGenre = "kinderspel, mode"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Piet"}, itemPrice = 0, itemName = "FiFa13", itemDescr = "", itemState = "kapotgespeelt", itemImage = "game_7.jpg", itemCategory = Game {gamePlatform = "PS3", gameYear = 2011, gameDeveloper = "voetbal Games", gameGenre = "Sport, voetbal"}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "martijn"}, itemPrice = 0, itemName = "Boormachine", itemDescr = "Het krachtige en compacte toestel Krachtig motor van 600 Watt, ideaal om te boren tot 10 mm boordiameter in metaal Bevestiging van boorspil in het lager voor hoge precisie Compact design en gering gewicht voor optimale bediening bij middelzware boortoepassingen Besturings-electronic voor exact aanboren Metalen snelspanboorhouder voor hoge precisie en lange levensduur Rechts- en linksdraaien Bijzonder geschikt voor boorgaten tot 10 mm in staal Functies: Rechts- en linksdraaien Electronic Softgrip Leveromvang: Snelspanboorhouder 1 - 10 mm", itemState = "Goed", itemImage = "tool_1.jpg", itemCategory = Tool {toolBrand = "Bosch", toolType = "BDF343SHE", toolYear = 2005}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Henny"}, itemPrice = 0, itemName = "Klopboormachine", itemDescr = "Veelzijdig - zagen, snijden, slijpen dankzij het oscillatieprincipe en de accessoires", itemState = "goed", itemImage = "tool_2.jpg", itemCategory = Tool {toolBrand = "Bosch", toolType = "GSB 20-2 re", toolYear = 2011}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Matt"}, itemPrice = 0, itemName = "bladblazer", itemDescr = "op benzine", itemState = "matig", itemImage = "tool_3.jpg", itemCategory = Tool {toolBrand = "ferm", toolType = "benzine", toolYear = 2006}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Hans"}, itemPrice = 0, itemName = "Kettingzaag", itemDescr = "op benzine", itemState = "goed", itemImage = "tool_4.jpg", itemCategory = Tool {toolBrand = "HD", toolType = "benzine", toolYear = 2001}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Frans"}, itemPrice = 0, itemName = "Zware klopboormachine", itemDescr = "electrisch, tot 25 mm en 350 mm lengte", itemState = "goed", itemImage = "tool_5.jpg", itemCategory = Tool {toolBrand = "Milwaukee", toolType = "Q1234", toolYear = 2003}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Frits"}, itemPrice = 0, itemName = "Multitool", itemDescr = "Veelzijdig - zagen, snijden, slijpen dankzij het oscillatieprincipe en de accessoires", itemState = "goed", itemImage = "tool_6.jpg", itemCategory = Tool {toolBrand = "Bosch", toolType = "PMF 10,8 LI", toolYear = 2011}, itemBorrowed = Nothing}
        , Item {itemId = ItemId (-1), itemOwner = LenderId {lenderIdLogin = "Piet"}, itemPrice = 0, itemName = "Heggenschaar", itemDescr = "Benzine, 90mm blad", itemState = "goed", itemImage = "tool_7.jpg", itemCategory = Tool {toolBrand = "Start", toolType = "Benzine", toolYear = 2012}, itemBorrowed = Nothing}
        ]