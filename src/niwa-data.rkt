#lang racket/base

(provide
 niwa-data)

;; wikiname, niwa-name, url, logo-url
(define niwa-data
  '((("arms" "armsgame")
     "ARMS Institute"
     "https://armswiki.org/wiki/Home"
     "/images/logos/armswiki.png"
     "ARMS Institute is a comprehensive resource for information about the Nintendo Switch game, ARMS. Founded on May 1, 2017 and growing rapidly, the wiki strives to offer in-depth coverage of ARMS from both a competitive and casual perspective. Join us and ARM yourself with knowledge!")
    (("pokemon" "monster")
     "Bulbapedia"
     "https://bulbapedia.bulbagarden.net/wiki/Main_Page"
     "/images/logos/bulbapedia.png"
     "A part of the Bulbagarden community, Bulbapedia was founded on December 21, 2004 by Liam Pomfret. Everything you need to know about Pokémon can be found at Bulbapedia, whether about the games, the anime, the manga, or something else entirely. With its Bulbanews section and the Bulbagarden forums, it's your one-stop online place for Pokémon.")
    (("dragalialost")
     "Dragalia Lost Wiki"
     "https://dragalialost.wiki/w/Dragalia_Lost_Wiki"
     "/images/logos/dragalialost.png"
     "The Dragalia Lost Wiki was originally founded in September 2018 on the Gamepedia platform but went independent in January 2021. The Wiki aims to document anything and everything Dragalia Lost, from in-game data to mechanics, story, guides, and more!")
    (("dragonquest")
     "Dragon Quest Wiki"
     "https://dragon-quest.org/wiki/Main_Page"
     "/images/logos/dragonquestwiki.png"
     "Originally founded on Wikia, the Dragon Quest Wiki was largely inactive until FlyingRagnar became an admin in late 2009. The wiki went independent about a year later when it merged with the Dragon Quest Dictionary/Encyclopedia which was run by Zenithian and supported by the Dragon's Den.  The Dragon Quest Wiki aims to be the most complete resource for Dragon Quest information on the web. It continues to grow in the hope that one day the series will be as popular in the rest of the world as it is in Japan.")
    (("fireemblem")
     "Fire Emblem Wiki"
     "https://fireemblemwiki.org/wiki/Main_Page"
     "/images/logos/fireemblemwiki.png"
     "Growing since August 26, 2010, Fire Emblem Wiki is a project whose goal is to cover all information pertaining to the Fire Emblem series. It aspires to become the most complete and accurate independent source of information on this series.")
    (("fzero" "f-zero")
     "F-Zero Wiki"
     "https://mutecity.org/wiki/F-Zero_Wiki"
     "/images/logos/fzerowiki.png"
     "Founded on Wikia in November 2007, F-Zero Wiki became independent with NIWA's help in 2011. F-Zero Wiki is quickly growing into the Internet's definitive source for the world of 2200 km/h+, from pilots to machines, and is the founding part of MuteCity.org, the web's first major F-Zero community.")
    (("goldensun")
     "Golden Sun Universe"
     "https://www.goldensunwiki.net/wiki/Main_Page"
     "/images/logos/goldensununiverse.png"
     "Originally founded on Wikia in late 2006, Golden Sun Universe has always worked hard to meet one particular goal: to be the single most comprehensive yet accessible resource on the Internet for Nintendo's RPG series Golden Sun. It became an independent wiki four years later. Covering characters and plot, documenting all aspects of the gameplay, featuring walkthroughs both thorough and bare-bones, and packed with all manner of odd and fascinating minutiae, Golden Sun Universe leaves no stone unturned!")
    (("tetris")
     "Hard Drop - Tetris Wiki"
     "https://harddrop.com/wiki/Main_Page"
     "/images/logos/harddrop.png"
     "The Tetris Wiki was founded by Tetris fans for Tetris fans on tetrisconcept.com in March 2006. The Tetris Wiki torch was passed to harddrop.com in July 2009. Hard Drop is a Tetris community for all Tetris players, regardless of skill or what version of Tetris you play.")
    (("kidicarus")
     "Icaruspedia"
     "https://www.kidicaruswiki.org/wiki/Main_Page"
     "/images/logos/icaruspedia.png"
     "Icaruspedia is the Kid Icarus wiki that keeps flying to new heights. After going independent on January 8, 2012, Icaruspedia has worked to become the largest and most trusted independent source of Kid Icarus information. Just like Pit, they\"ll keep on fighting until the job is done.")
    (("splatoon" "uk-splatoon" "splatoon3" "splatoon2")
     "Inkipedia"
     "https://splatoonwiki.org/wiki/Main_Page"
     "/images/logos/inkipedia.png"
     "Inkipedia is your ever-growing go-to source for all things Splatoon related. Though founded on Wikia on June 10, 2014, Inkipedia went independent on May 18, 2015, just days before Splatoon's release. Our aim is to cover all aspects of the series, both high and low. Come splat with us now!")
    (("starfox")
     "Lylat Wiki"
     "https://starfoxwiki.info/wiki/Lylat_Wiki"
     "/images/logos/lylatwiki.png"
     "Out of seemingly nowhere, Lylat Wiki sprung up one day in early 2010. Led by creator, Justin Folvarcik, and project head, Tacopill, the wiki has reached stability since the move to its own domain. The staff of Lylat Wiki are glad to help out the NIWA wikis and are even prouder to join NIWA's ranks as the source for information on the Star Fox series.")
    (("metroid" "themetroid")
     "Metroid Wiki"
     "https://www.metroidwiki.org/wiki/Main_Page"
     "/images/logos/metroidwiki.png"
     "Metroid Wiki, founded on January 27, 2010 by Nathanial Rumphol-Janc and Zelda Informer, is a rapidly expanding wiki that covers everything Metroid, from the games, to every suit, vehicle and weapon.")
    (("nintendo" "nintendoseries" "nintendogames")
     "Nintendo Wiki"
     "http://niwanetwork.org/wiki/Main_Page"
     "/images/logos/nintendowiki.png"
     "Created on May 12, 2010, NintendoWiki (N-Wiki) is a collaborative project by the NIWA team to create an encyclopedia dedicated to Nintendo, being the company around which all other NIWA content is focused. It ranges from mainstream information such as the games and people who work for the company, to the most obscure info like patents and interesting trivia.")
    (("animalcrossing" "animalcrossingcf" "acnh")
     "Nookipedia"
     "https://nookipedia.com/wiki/Main_Page"
     "/images/logos/nookipedia.png"
     "Founded in August 2005 on Wikia, Nookipedia was originally known as Animal Crossing City. Shortly after its five-year anniversary, Animal Crossing City decided to merge with the independent Animal Crossing Wiki, which in January 2011 was renamed to Nookipedia. Covering everything from the series including characters, items, critters, and much more, Nookipedia is your number one resource for everything Animal Crossing!")
    (("pikmin")
     "Pikipedia"
     "https://www.pikminwiki.com/"
     "/images/logos/pikipedia.png"
     "Pikipedia, also known as Pikmin Wiki, was founded by Dark Lord Revan on Wikia in December 2005. In September 2010, with NIWA's help, Pikipedia moved away from Wikia to become independent. Pikipedia is working towards their goal of being the foremost source for everything Pikmin.")
    (("pikmin-fan" "pikpikpedia")
     "Pimkin Fanon"
     "https://www.pikminfanon.com/wiki/Main_Page"
     "/images/logos/pikifanon.png"
     "Pikmin Fanon is a Pikmin wiki for fan stories (fanon). Founded back on November 1, 2008 by Rocky0718 as a part of Wikia, Pikmin Fanon has been independent since September 14, 2010. Check them out for fan created stories based around the Pikmin series.")
    (("supersmashbros")
     "SmashWiki"
     "https://www.ssbwiki.com/"
     "/images/logos/smashwiki.png"
     "Originally two separate wikis (one on SmashBoards, the other on Wikia), SmashWiki as we know it was formed out of a merge on February 29th, 2008, becoming independent on September 28th, 2010. SmashWiki is the premier source of Smash Bros. information, from simple tidbits to detailed mechanics, and also touches on the origins of its wealth of content from its sibling franchises.")
    (("starfy")
     "Starfy Wiki"
     "https://www.starfywiki.org/wiki/Main_Page"
     "/images/logos/starfywiki.png"
     "Founded on May 30, 2009, Starfy Wiki's one goal is to become the best source on Nintendo's elusive game series The Legendary Starfy. After gaining independence in 2011 with the help of Tappy and the wiki's original administrative team, the wiki still hopes to achieve its goal and be the best source of Starfy info for all present and future fans.")
    (()
     "StrategyWiki"
     "https://www.strategywiki.org/wiki/Main_Page"
     "/images/logos/strategywiki.png"
     "StrategyWiki was founded in December 2005 by former member Brandon Suit with the idea that the existing strategy guides on the Internet could be improved. Three years later, in December 2008, Scott Jacobi officially established Abxy LLC for the purpose of owning and operating StrategyWiki as a community. Their vision is to bring free, collaborative video game strategy guides to the masses, including Nintendo franchise strategy guides.")
    (("mario" "themario" "imario" "supermarionintendo" "mariokart" "luigi-kart" "mario3")
     "Super Mario Wiki"
     "https://www.mariowiki.com/"
     "/images/logos/mariowiki.png"
     "Online since August 12, 2005, when it was founded by Steve Shinn, Super Mario Wiki has you covered for anything Mario, Donkey Kong, Wario, Luigi, Yoshi—the whole gang, in fact. With its own large community in its accompanying forum, Super Mario Wiki is not only a great encyclopedia, but a fansite for you to talk anything Mario.")
    (("mario64")
     "Ukikipedia"
     "https://ukikipedia.net/wiki/Main_Page"
     "/images/logos/ukikipedia.png"
     "Founded in 2018, Ukikipedia is a wiki focused on expert level knowledge of Super Mario 64, including detailed coverage of game mechanics, glitches, speedrunning, and challenges.")
    (("advancewars")
     "Wars Wiki"
     "https://www.warswiki.org/wiki/Main_Page"
     "/images/logos/warswiki.png"
     "Created in February 2009, Wars Wiki is a small wiki community with a large heart. Founded by JoJo and Wars Central, Wars Wiki is going strong on one of Nintendo's lesser known franchises. Wars Wiki is keen to contribute to NIWA, and we're proud to be able to support them. With the Wars Central community, including forums, it's definitely worth checking out.")
    (("earthbound")
     "WikiBound"
     "https://www.wikibound.info/wiki/WikiBound"
     "/images/logos/wikibound.png"
     "Founded in early 2010 by Tacopill, WikiBound strives to create a detailed database on the Mother/EarthBound games, a quaint series only having two games officially released outside of Japan. Help spread the PK Love by editing WikiBound!")
    (("kirby")
     "WiKirby"
     "https://wikirby.com/wiki/Kirby_Wiki"
     "/images/logos/wikirby.png"
     "WiKirby. It's a wiki. About Kirby! Amidst the excitement of NIWA being founded, Josh LeJeune decided to create a Kirby Wiki, due to lack of a strong independent one online. Coming online on January 24, 2010, WiKirby continues its strong launch with a dedicated community and a daily growing source of Kirby based knowledge.")
    (("xenoblade" "xenoseries" "xenogears" "xenosaga")
     "Xeno Series Wiki"
     "https://www.xenoserieswiki.org/wiki/Main_Page"
     "/images/logos/xenoserieswiki.png"
     "Xeno Series Wiki was created February 4, 2020 by Sir Teatei Moonlight. While founded by the desire to have an independent wiki for Xenoblade, there was an interest in including the Xenogears and Xenosaga games within its focus as well. This wide range of coverage means it's always in need of new editors to help bolster its many subjects.")
    (("zelda" "zelda-archive")
     "Zeldapedia"
     "https://zeldapedia.wiki/wiki/Main_Page"
     "/images/logos/zeldapedia.png"
     "Founded on April 23, 2005 as Zelda Wiki, today's Zeldapedia is your definitive source for encyclopedic information on The Legend of Zelda series, as well as all of the latest Zelda news.")))

;; get the current dataset so it can be stored above
(module+ test
  (require racket/generator
           racket/list
           net/http-easy
           html-parsing
           "xexpr-utils.rkt")
  (define r (get "https://www.niwanetwork.org/members/"))
  (define x (html->xexp (bytes->string/utf-8 (response-body r))))
  (define english ((query-selector (λ (e a c) (println a) (equal? (get-attribute 'id a) "content1")) x)))
  (define gen (query-selector (λ (e a c) (has-class? "member" a)) english))
  (for/list ([item (in-producer gen #f)])
    (define links (query-selector (λ (e a c) (eq? e 'a)) item))
    (define url (get-attribute 'href (bits->attributes (links))))
    (define title (third (links)))
    (define icon (get-attribute 'src (bits->attributes ((query-selector (λ (e a c) (eq? e 'img)) item)))))
    (define description (second ((query-selector (λ (e a c) (eq? e 'p)) item))))
    (list '() title url icon description)))
