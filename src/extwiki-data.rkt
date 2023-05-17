#lang racket/base

(provide
 (struct-out extwiki-props^)
 (struct-out extwiki-group^)
 extwiki-groups
 (struct-out extwiki^)
 extwikis)

(struct extwiki-props^ (go) #:transparent)

(struct extwiki-group^ (name links description) #:transparent)
(define extwiki-groups
  (hasheq 'NIWA
          (extwiki-group^
           "NIWA"
           '(("Why did editors leave Fandom?" . "https://www.kotaku.com.au/2022/10/massive-zelda-wiki-reclaims-independence-six-months-before-tears-of-the-kingdom/"))
           (λ (props)
             `(p "Most major Nintendo wikis are part of the "
                 (a (@ (href "https://www.niwanetwork.org/about/")) "Nintendo Independent Wiki Alliance")
                 " and have their own wikis off Fandom.")))

          'SEIWA
          (extwiki-group^
           "SEIWA"
           '(("SEIWA Website" . "https://seiwanetwork.org/"))
           (λ (props)
             `(p "The Square Enix Indpendent Wiki Alliance, or SEIWA, is a network of independent wikis established in 2011 and focused on providing high-quality coverage of Square Enix and its content. We work together, along with our affiliates and others, to co-operate and support one another while providing the best-quality content on the various Square Enix video games and media.")))

          'Terraria
          (extwiki-group^
           "Terraria"
           '(("Announcement: New Official Terraria Wiki!" . "https://forums.terraria.org/index.php?threads/new-official-terraria-wiki-launches-today.111239/") ("In the media" . "https://www.pcgamesn.com/terraria/wiki"))
           (λ (props) '()))

          'Calamity_Mod
          (extwiki-group^
           "Calamity Mod"
           '(("Announcement: Moving to wiki.gg" . "https://www.reddit.com/r/CalamityMod/comments/ts0586/important_calamity_wiki_announcement/"))
           (λ (props) '()))

          'ARK
          (extwiki-group^
           "ARK"
           '(("Announcement: Official Wiki Is Moving!" . "https://survivetheark.com/index.php?/forums/topic/657902-official-ark-wiki-feedback/")
             ("Reasons" . "https://todo.sr.ht/~cadence/breezewiki-todo/4#event-216613")
             ("Browser Extension" . "https://old.reddit.com/r/playark/comments/xe51sy/official_ark_wiki_launched_a_browser_extension_to/"))
           (λ (props) '()))

          'Astroneer
          (extwiki-group^
           "Astroneer"
           '(("Migration discussion" . "https://old.reddit.com/r/Astroneer/comments/z905id/the_official_astroneer_wiki_has_moved_to_wikigg/") ("Migration info" . "https://astroneer.fandom.com/wiki/Talk:Astroneer_Wiki/Migration_to_Wiki.gg"))
           (λ (props) '()))

          'RuneScape
          (extwiki-group^
           "RuneScape"
           '(("Leaving Wikia" . "https://runescape.wiki/w/Forum:Leaving_Wikia")
             ("In the media" . "https://kotaku.com/video-game-wikis-abandon-their-platform-after-year-of-p-1829401866")
             ("Browser Extension" . "https://runescape.wiki/w/RuneScape:Finding_the_wikis_with_ease#Extensions"))
           (λ (props) '()))

          'empty
          (extwiki-group^
           "Misc"
           '(("This wiki doesn't have a description yet. Add one?" . "https://docs.breezewiki.com/Reporting_Bugs.html"))
           #f)))

;; wikiname, niwa-name, url, logo-url
(struct extwiki^ (wikinames banner group name home logo description) #:transparent)
(define extwikis
  (list
   (extwiki^
    '("arms" "armsgame") 'default
    'NIWA
    "ARMS Institute"
    "https://armswiki.org/wiki/Home"
    "https://niwanetwork.org/images/logos/armswiki.png"
    (λ (props)
      `((p "ARMS Institute is a comprehensive resource for information about the Nintendo Switch game, ARMS. Founded on May 1, 2017 and growing rapidly, the wiki strives to offer in-depth coverage of ARMS from both a competitive and casual perspective. Join us and ARM yourself with knowledge!"))))

   (extwiki^
    '("pokemon" "monster") 'default
    'NIWA
    "Bulbapedia"
    "https://bulbapedia.bulbagarden.net/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/bulbapedia.png"
    (λ (props)
      `((p "A part of the Bulbagarden community, Bulbapedia was founded on December 21, 2004 by Liam Pomfret. Everything you need to know about Pokémon can be found at Bulbapedia, whether about the games, the anime, the manga, or something else entirely. With its Bulbanews section and the Bulbagarden forums, it's your one-stop online place for Pokémon."))))

   (extwiki^
    '("dragalialost") 'default
    'NIWA
    "Dragalia Lost Wiki"
    "https://dragalialost.wiki/w/Dragalia_Lost_Wiki"
    "https://niwanetwork.org/images/logos/dragalialost.png"
    (λ (props)
      `((p "The Dragalia Lost Wiki was originally founded in September 2018 on the Gamepedia platform but went independent in January 2021. The Wiki aims to document anything and everything Dragalia Lost, from in-game data to mechanics, story, guides, and more!"))))

   (extwiki^
    '("dragonquest") 'default
    'NIWA
    "Dragon Quest Wiki"
    "https://dragon-quest.org/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/dragonquestwiki.png"
    (λ (props)
      `((p "Originally founded on Wikia, the Dragon Quest Wiki was largely inactive until FlyingRagnar became an admin in late 2009. The wiki went independent about a year later when it merged with the Dragon Quest Dictionary/Encyclopedia which was run by Zenithian and supported by the Dragon's Den.  The Dragon Quest Wiki aims to be the most complete resource for Dragon Quest information on the web. It continues to grow in the hope that one day the series will be as popular in the rest of the world as it is in Japan."))))

   (extwiki^
    '("fireemblem") 'default
    'NIWA
    "Fire Emblem Wiki"
    "https://fireemblemwiki.org/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/fireemblemwiki.png"
    (λ (props)
      `((p "Growing since August 26, 2010, Fire Emblem Wiki is a project whose goal is to cover all information pertaining to the Fire Emblem series. It aspires to become the most complete and accurate independent source of information on this series."))))

   (extwiki^
    '("fzero" "f-zero") 'default
    'NIWA
    "F-Zero Wiki"
    "https://mutecity.org/wiki/F-Zero_Wiki"
    "https://niwanetwork.org/images/logos/fzerowiki.png"
    (λ (props)
      `((p "Founded on Wikia in November 2007, F-Zero Wiki became independent with NIWA's help in 2011. F-Zero Wiki is quickly growing into the Internet's definitive source for the world of 2200 km/h+, from pilots to machines, and is the founding part of MuteCity.org, the web's first major F-Zero community."))))

   (extwiki^
    '("goldensun") 'default
    'NIWA
    "Golden Sun Universe"
    "https://www.goldensunwiki.net/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/goldensununiverse.png"
    (λ (props)
      `((p "Originally founded on Wikia in late 2006, Golden Sun Universe has always worked hard to meet one particular goal: to be the single most comprehensive yet accessible resource on the Internet for Nintendo's RPG series Golden Sun. It became an independent wiki four years later. Covering characters and plot, documenting all aspects of the gameplay, featuring walkthroughs both thorough and bare-bones, and packed with all manner of odd and fascinating minutiae, Golden Sun Universe leaves no stone unturned!"))))

   (extwiki^
    '("tetris") 'default
    'NIWA
    "Hard Drop - Tetris Wiki"
    "https://harddrop.com/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/harddrop.png"
    (λ (props)
      `((p "The Tetris Wiki was founded by Tetris fans for Tetris fans on tetrisconcept.com in March 2006. The Tetris Wiki torch was passed to harddrop.com in July 2009. Hard Drop is a Tetris community for all Tetris players, regardless of skill or what version of Tetris you play."))))

   (extwiki^
    '("kidicarus") 'default
    'NIWA
    "Icaruspedia"
    "https://www.kidicaruswiki.org/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/icaruspedia.png"
    (λ (props)
      `((p "Icaruspedia is the Kid Icarus wiki that keeps flying to new heights. After going independent on January 8, 2012, Icaruspedia has worked to become the largest and most trusted independent source of Kid Icarus information. Just like Pit, they'll keep on fighting until the job is done."))))

   (extwiki^
    '("splatoon" "uk-splatoon" "splatoon3" "splatoon2") 'default
    'NIWA
    "Inkipedia"
    "https://splatoonwiki.org/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/inkipedia.png"
    (λ (props)
      `((p "Inkipedia is your ever-growing go-to source for all things Splatoon related. Though founded on Wikia on June 10, 2014, Inkipedia went independent on May 18, 2015, just days before Splatoon's release. Our aim is to cover all aspects of the series, both high and low. Come splat with us now!"))))

   (extwiki^
    '("starfox") 'default
    'NIWA
    "Lylat Wiki"
    "https://starfoxwiki.info/wiki/Lylat_Wiki"
    "https://niwanetwork.org/images/logos/lylatwiki.png"
    (λ (props)
      `((p "Out of seemingly nowhere, Lylat Wiki sprung up one day in early 2010. Led by creator, Justin Folvarcik, and project head, Tacopill, the wiki has reached stability since the move to its own domain. The staff of Lylat Wiki are glad to help out the NIWA wikis and are even prouder to join NIWA's ranks as the source for information on the Star Fox series."))))

   (extwiki^
    '("metroid" "themetroid") 'default
    'NIWA
    "Metroid Wiki"
    "https://www.metroidwiki.org/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/metroidwiki.png"
    (λ (props)
      `((p "Metroid Wiki, founded on January 27, 2010 by Nathanial Rumphol-Janc and Zelda Informer, is a rapidly expanding wiki that covers everything Metroid, from the games, to every suit, vehicle and weapon."))))

   (extwiki^
    '("nintendo" "nintendoseries" "nintendogames") 'default
    'NIWA
    "Nintendo Wiki"
    "http://niwanetwork.org/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/nintendowiki.png"
    (λ (props)
      `((p "Created on May 12, 2010, NintendoWiki (N-Wiki) is a collaborative project by the NIWA team to create an encyclopedia dedicated to Nintendo, being the company around which all other NIWA content is focused. It ranges from mainstream information such as the games and people who work for the company, to the most obscure info like patents and interesting trivia."))))

   (extwiki^
    '("animalcrossing" "animalcrossingcf" "acnh") 'default
    'NIWA
    "Nookipedia"
    "https://nookipedia.com/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/nookipedia.png"
    (λ (props)
      `((p "Founded in August 2005 on Wikia, Nookipedia was originally known as Animal Crossing City. Shortly after its five-year anniversary, Animal Crossing City decided to merge with the independent Animal Crossing Wiki, which in January 2011 was renamed to Nookipedia. Covering everything from the series including characters, items, critters, and much more, Nookipedia is your number one resource for everything Animal Crossing!"))))

   (extwiki^
    '("pikmin") 'default
    'NIWA
    "Pikipedia"
    "https://www.pikminwiki.com/"
    "https://niwanetwork.org/images/logos/pikipedia.png"
    (λ (props)
      `((p "Pikipedia, also known as Pikmin Wiki, was founded by Dark Lord Revan on Wikia in December 2005. In September 2010, with NIWA's help, Pikipedia moved away from Wikia to become independent. Pikipedia is working towards their goal of being the foremost source for everything Pikmin."))))

   (extwiki^
    '("pikmin-fan" "pikpikpedia") 'default
    'NIWA
    "Pimkin Fanon"
    "https://www.pikminfanon.com/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/pikifanon.png"
    (λ (props)
      `((p "Pikmin Fanon is a Pikmin wiki for fan stories (fanon). Founded back on November 1, 2008 by Rocky0718 as a part of Wikia, Pikmin Fanon has been independent since September 14, 2010. Check them out for fan created stories based around the Pikmin series."))))

   (extwiki^
    '("supersmashbros") 'default
    'NIWA
    "SmashWiki"
    "https://www.ssbwiki.com/"
    "https://niwanetwork.org/images/logos/smashwiki.png"
    (λ (props)
      `((p "Originally two separate wikis (one on SmashBoards, the other on Wikia), SmashWiki as we know it was formed out of a merge on February 29th, 2008, becoming independent on September 28th, 2010. SmashWiki is the premier source of Smash Bros. information, from simple tidbits to detailed mechanics, and also touches on the origins of its wealth of content from its sibling franchises."))))

   (extwiki^
    '("starfy") 'default
    'NIWA
    "Starfy Wiki"
    "https://www.starfywiki.org/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/starfywiki.png"
    (λ (props)
      `((p "Founded on May 30, 2009, Starfy Wiki's one goal is to become the best source on Nintendo's elusive game series The Legendary Starfy. After gaining independence in 2011 with the help of Tappy and the wiki's original administrative team, the wiki still hopes to achieve its goal and be the best source of Starfy info for all present and future fans."))))

   (extwiki^
    '() 'default
    'NIWA
    "StrategyWiki"
    "https://www.strategywiki.org/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/strategywiki.png"
    (λ (props)
      `((p "StrategyWiki was founded in December 2005 by former member Brandon Suit with the idea that the existing strategy guides on the Internet could be improved. Three years later, in December 2008, Scott Jacobi officially established Abxy LLC for the purpose of owning and operating StrategyWiki as a community. Their vision is to bring free, collaborative video game strategy guides to the masses, including Nintendo franchise strategy guides."))))

   (extwiki^
    '("mario" "themario" "imario" "supermarionintendo" "mariokart" "luigi-kart" "mario3") 'default
    'NIWA
    "Super Mario Wiki"
    "https://www.mariowiki.com/"
    "https://niwanetwork.org/images/logos/mariowiki.png"
    (λ (props)
      `((p "Online since August 12, 2005, when it was founded by Steve Shinn, Super Mario Wiki has you covered for anything Mario, Donkey Kong, Wario, Luigi, Yoshi—the whole gang, in fact. With its own large community in its accompanying forum, Super Mario Wiki is not only a great encyclopedia, but a fansite for you to talk anything Mario."))))

   (extwiki^
    '("mario64") 'default
    'NIWA
    "Ukikipedia"
    "https://ukikipedia.net/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/ukikipedia.png"
    (λ (props)
      `((p "Founded in 2018, Ukikipedia is a wiki focused on expert level knowledge of Super Mario 64, including detailed coverage of game mechanics, glitches, speedrunning, and challenges."))))

   (extwiki^
    '("advancewars") 'default
    'NIWA
    "Wars Wiki"
    "https://www.warswiki.org/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/warswiki.png"
    (λ (props)
      `((p "Created in February 2009, Wars Wiki is a small wiki community with a large heart. Founded by JoJo and Wars Central, Wars Wiki is going strong on one of Nintendo's lesser known franchises. Wars Wiki is keen to contribute to NIWA, and we're proud to be able to support them. With the Wars Central community, including forums, it's definitely worth checking out."))))

   (extwiki^
    '("earthbound") 'default
    'NIWA
    "WikiBound"
    "https://www.wikibound.info/wiki/WikiBound"
    "https://niwanetwork.org/images/logos/wikibound.png"
    (λ (props)
      `((p "Founded in early 2010 by Tacopill, WikiBound strives to create a detailed database on the Mother/EarthBound games, a quaint series only having two games officially released outside of Japan. Help spread the PK Love by editing WikiBound!"))))

   (extwiki^
    '("kirby") 'default
    'NIWA
    "WiKirby"
    "https://wikirby.com/wiki/Kirby_Wiki"
    "https://niwanetwork.org/images/logos/wikirby.png"
    (λ (props)
      `((p "WiKirby. It's a wiki. About Kirby! Amidst the excitement of NIWA being founded, Josh LeJeune decided to create a Kirby Wiki, due to lack of a strong independent one online. Coming online on January 24, 2010, WiKirby continues its strong launch with a dedicated community and a daily growing source of Kirby based knowledge."))))

   (extwiki^
    '("xenoblade" "xenoseries" "xenogears" "xenosaga") 'parallel
    'NIWA
    "Xeno Series Wiki"
    "https://www.xenoserieswiki.org/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/xenoserieswiki.png"
    (λ (props)
      `((p "Xeno Series Wiki was created February 4, 2020 by Sir Teatei Moonlight. While founded by the desire to have an independent wiki for Xenoblade, there was an interest in including the Xenogears and Xenosaga games within its focus as well. This wide range of coverage means it's always in need of new editors to help bolster its many subjects."))))

   (extwiki^
    '("zelda" "zelda-archive") 'default
    'NIWA
    "Zeldapedia"
    "https://zeldapedia.wiki/wiki/Main_Page"
    "https://niwanetwork.org/images/logos/zeldapedia.png"
    (λ (props)
      `((p "Founded on April 23, 2005 as Zelda Wiki, today's Zeldapedia is your definitive source for encyclopedic information on The Legend of Zelda series, as well as all of the latest Zelda news. Zeldapedia went independent from Fandom in October 2022, citing Fandom's recent buyouts and staffing decisions among their reasons."))))

   (extwiki^
    '("chrono") 'default
    'SEIWA
    "Chrono Wiki"
    "https://www.chronowiki.org/wiki/Chrono_Wiki"
    "https://cdn.wikimg.net/en/chronowiki/images/5/59/Site-wiki.png"
    (λ (props) '((p "A free encyclopedia dedicated to Chrono Trigger, Chrono Cross, Radical Dreamers, and everything else related to the series. A long, rich history and a friendly, encouraging userbase makes this the best Chrono in the entire time/space continuum!"))))

   (extwiki^
    '("finalfantasy" "finalfantasyxv" "ffxiclopedia") 'parallel
    'SEIWA
    "Final Fantasy Wiki"
    "https://finalfantasywiki.com/wiki/Main_Page"
    "https://cdn.finalfantasywiki.com/wiki.png"
    (λ (props) '((p "A new wiki focused on covering Square Enix's flagship franchise, the critically-acclaimed Final Fantasy series. The Final Fantasy Wiki was founded on January 12, 2020 as part of SEIWA and covers all things Final Fantasy and related franchises."))))

   (extwiki^
    '("kingdomhearts") 'default
    'SEIWA
    "Kingdom Hearts Wiki"
    "https://www.khwiki.com/"
    "https://kh.wiki.gallery/images/b/bc/Wiki.png"
    (λ (props) '((p "The Kingdom Hearts Wiki attempts to document all things related to the Kingdom Hearts series, from elements of storyline to gameplay. The site was originally founded on April 1, 2006 on Wikia and became independent on February 9, 2011. Since this time, the community of the KHWiki strives to be the most professional and comprehensive Kingdom Hearts resource in the world."))))

   (extwiki^
    '("squareenix") 'default
    'SEIWA
    "Square Enix Wiki"
    "https://wiki.seiwanetwork.org/wiki/Main_Page"
    "https://cdn.seiwanetwork.org/thumb/9/94/Square_Enix_Wiki_Logo.png/200px-Square_Enix_Wiki_Logo.png"
    (λ (props) '((p "The Square Enix Wiki was founded on February 8, 2012, and is an up-and-coming wiki project created by SEIWA. It focuses on covering all things Square Enix, from its video game series to its physical publications to its most notable employees and work as a company."))))

   (extwiki^
    '("terraria") 'default
    'Terraria
    "Official Terraria Wiki"
    "https://terraria.wiki.gg/wiki/Terraria_Wiki"
    "https://terraria.wiki.gg/images/5/5a/App_icon_1.3_Update.png"
    (λ (props)
      `()))

   (extwiki^
    '("calamitymod" "calamity-mod") 'empty
    'Calamity_Mod
    "Official Calamity Mod Wiki"
    "https://calamitymod.wiki.gg/wiki/Calamity_Mod_Wiki"
    #f
    #f)

   (extwiki^
    '("ark" "ark-survival-evolved-archive") 'default
    'ARK
    "ARK Community Wiki"
    "https://ark.wiki.gg/wiki/ARK_Survival_Evolved_Wiki"
    "https://ark.wiki.gg/images/e/e6/Site-logo.png"
    (λ (props)
      `((p "The official ARK: Survival Evolved Wiki launched in 2016. In April 2022 it moved to wiki.gg's hosting to improve creative control and the overall browsing experience."))))

   (extwiki^
    '("runescape") 'default
    'RuneScape
    "RuneScape Wiki"
    "https://runescape.wiki/"
    "https://runescape.wiki/images/Wiki.png"
    (λ (props)
      `((p "The RuneScape Wiki was founded on April 8, 2005. In October 2018, the wiki left Fandom (then Wikia), citing their apathy towards the wiki and excessive advertisements."))))

   (extwiki^
    '("oldschoolrunescape") 'default
    'RuneScape
    "Old School RuneScape Wiki"
    "https://oldschool.runescape.wiki/"
    "https://oldschool.runescape.wiki/images/Wiki.png"
    (λ (props)
      `((p "The Old School RuneScape Wiki was founded on February 14, 2013. In October 2018, the RuneScape Wiki left Fandom (then Wikia), citing their apathy towards the wiki and excessive advertisements, with the Old School RuneScape Wiki following suit."))))

   (extwiki^
    '("runescapeclassic") 'default
    'RuneScape
    "RuneScape Classic Wiki"
    "https://classic.runescape.wiki/"
    "https://classic.runescape.wiki/images/Wiki.png"
    (λ (props)
      `((p "The Old School RuneScape Wiki was founded on April 19, 2009. In October 2018, the RuneScape Wiki left Fandom (then Wikia), citing their apathy towards the wiki and excessive advertisements, with the RuneScape Classic Wiki following suit."))))

   (extwiki^
    '("astroneer") 'default
    'Astroneer
    "Astroneer Wiki"
    "https://astroneer.wiki.gg/wiki/Astroneer_Wiki"
    "https://astroneer.wiki.gg/images/7/74/Icon_Astroneer.png"
    (λ (props)
      `((p "“Fandom bought Gamepedia and forced a migration, with their restricted, ad-heavy appearance, and other annoying features that we could not remove, the wiki grew slow and annoying to use, especially for logged out users.")
        (p "“We decided to move away from Fandom to Wiki.gg, which returns the wiki to how it used to be on gamepedia, without the ads spamming and forced videos.”"))))

   ;; fandom wikinames * empty * empty * Name * Home Page
   (extwiki^ '("aether") 'empty 'empty "Aether Wiki" "https://aether.wiki.gg/wiki/Aether_Wiki" #f #f)
   (extwiki^ '("before-darkness-falls") 'empty 'empty "Before Darkness Falls Wiki" "https://beforedarknessfalls.wiki.gg/wiki/Before_Darkness_Falls_Wiki" #f #f)
   (extwiki^ '("chivalry" "chivalry2") 'empty 'empty "Official Chivalry Wiki" "https://chivalry.wiki.gg/wiki/Chivalry_Wiki" #f #f)
   (extwiki^ '("clockup") 'empty 'empty "CLOCKUP WIKI" "https://en.clockupwiki.org/wiki/CLOCKUP_WIKI:Plan" #f #f)
   (extwiki^ '("half-life") 'empty 'empty "Combine OverWiki" "https://combineoverwiki.net/wiki/Main_Page" #f #f)
   (extwiki^ '("coromon") 'empty 'empty "Coromon Wiki" "https://coromon.wiki.gg/wiki/Coromon_Wiki" #f #f)
   (extwiki^ '("cosmoteer") 'empty 'empty "Cosmoteer Wiki" "https://cosmoteer.wiki.gg/wiki/Cosmoteer_Wiki" #f #f)
   (extwiki^ '("criticalrole") 'empty 'empty "Encylopedia Exandria" "https://criticalrole.miraheze.org/wiki/Main_Page" #f #f)
   (extwiki^ '("cuphead") 'empty 'empty "Cuphead Wiki" "https://cuphead.wiki.gg/wiki/Cuphead_Wiki" #f #f)
   (extwiki^ '("darkdeity") 'empty 'empty "Dark Deity Wiki" "https://darkdeity.wiki.gg/wiki/Dark_Deity_Wiki" #f #f)
   (extwiki^ '("deeprockgalactic") 'empty 'empty "Deep Rock Galactic Wiki" "https://deeprockgalactic.wiki.gg/wiki/Deep_Rock_Galactic_Wiki" #f #f)
   (extwiki^ '("doom") 'empty 'empty "DoomWiki.org" "https://doomwiki.org/wiki/Entryway" #f #f)
   (extwiki^ '("dreamscaper") 'empty 'empty "Official Dreamscaper Wiki" "https://dreamscaper.wiki.gg/wiki/Dreamscaper_Wiki" #f #f)
   (extwiki^ '("elderscrolls") 'empty 'empty "UESP" "https://en.uesp.net/wiki/Main_Page" #f #f)
   (extwiki^ '("fiend-folio") 'empty 'empty "Official Fiend Folio Wiki" "https://fiendfolio.wiki.gg/wiki/Fiend_Folio_Wiki" #f #f)
   (extwiki^ '("foxhole") 'empty 'empty "Foxhole Wiki" "https://foxhole.wiki.gg/wiki/Foxhole_Wiki" #f #f)
   (extwiki^ '("have-a-nice-death") 'empty 'empty "Have a Nice Death Wiki" "https://haveanicedeath.wiki.gg/wiki/Have_a_Nice_Death_Wiki" #f #f)
   (extwiki^ '("jojo" "jojos") 'empty 'empty "JoJo's Bizarre Encyclopedia" "https://jojowiki.com/" #f #f)
   (extwiki^ '("legiontd2") 'empty 'empty "Legion TD 2 Wiki" "https://legiontd2.wiki.gg/wiki/Legion_TD_2_Wiki" #f #f)
   (extwiki^ '("noita") 'empty 'empty "Noita Wiki" "https://noita.wiki.gg/wiki/Noita_Wiki" #f #f)
   (extwiki^ '("pathofexile") 'empty 'empty "Official Path of Exile Wiki" "https://www.poewiki.net/wiki/Path_of_Exile_Wiki" #f #f)
   (extwiki^ '("projectarrhythmia") 'empty 'empty "Project Arrhythmia Wiki" "https://projectarrhythmia.wiki.gg/wiki/Project_Arrhythmia_Wiki" #f #f)
   (extwiki^ '("sandsofaura") 'empty 'empty "Official Sands of Aura Wiki" "https://sandsofaura.wiki.gg/wiki/Sands_of_Aura_Wiki" #f #f)
   (extwiki^ '("seaofthieves") 'empty 'empty "Official Sea of Thieves Wiki" "https://seaofthieves.wiki.gg/wiki/Sea_of_Thieves" #f #f)
   (extwiki^ '("sonsoftheforest") 'empty 'empty "Sons of the Forest Wiki" "https://sonsoftheforest.wiki.gg/wiki/Sons_of_the_Forest_Wiki" #f #f)
   (extwiki^ '("stardewvalley") 'empty 'empty "Official Stardew Valley Wiki" "https://www.stardewvalleywiki.com/Stardew_Valley_Wiki" #f #f)
   (extwiki^ '("steamworld") 'empty 'empty "Official SteamWorld Wiki" "https://steamworld.wiki.gg/wiki/SteamWorld_Wiki" #f #f)
   (extwiki^ '("teamfortress") 'empty 'empty "Official Team Fortress Wiki" "https://wiki.teamfortress.com/wiki/Main_Page" #f #f)
   (extwiki^ '("temtem") 'empty 'empty "Official Temtem Wiki" "https://temtem.wiki.gg/wiki/Temtem_Wiki" #f #f)
   (extwiki^ '("terrariamods") 'empty 'empty "Official Terraria Mods Wiki" "https://terrariamods.wiki.gg/wiki/Terraria_Mods_Wiki" #f #f)
   (extwiki^ '("thoriummod") 'empty 'empty "Official Thorium Mod Wiki" "https://thoriummod.wiki.gg/wiki/Thorium_Mod_Wiki" #f #f)
   (extwiki^ '("totherescue") 'empty 'empty "To The Rescue!" "https://totherescue.wiki.gg/wiki/To_The_Rescue%21_Wiki" #f #f)
   (extwiki^ '("touhou") 'empty 'empty "Touhou Wiki" "https://en.touhouwiki.net/wiki/Touhou_Wiki" #f #f)
   (extwiki^ '("undermine") 'empty 'empty "Official UnderMine Wiki" "https://undermine.wiki.gg/wiki/UnderMine_Wiki" #f #f)
   (extwiki^ '("westofloathing" "loathing") 'empty 'empty "Wiki of Loathing" "https://loathing.wiki.gg/wiki/Wiki_of_Loathing" #f #f)
   (extwiki^ '("willyousnail") 'empty 'empty "Official Will You Snail Wiki" "https://willyousnail.wiki.gg/wiki/Will_You_Snail_Wiki" #f #f)
   (extwiki^ '("yumenikki" "yume-nikki-dream-diary") 'empty 'empty "Yume Wiki" "https://yume.wiki/Main_Page" #f #f)))

;; get the current dataset so it can be stored above
(module+ fetch
  (require racket/generator
           racket/list
           net/http-easy
           html-parsing
           "../lib/xexpr-utils.rkt")
  (define r (get "https://www.niwanetwork.org/members/"))
  (define x (html->xexp (bytes->string/utf-8 (response-body r))))
  (define english ((query-selector (λ (e a c) (equal? (get-attribute 'id a) "content1")) x)))
  (define gen (query-selector (λ (e a c) (has-class? "member" a)) english))
  (for/list ([item (in-producer gen #f)])
    (define links (query-selector (λ (e a c) (eq? e 'a)) item))
    (define url (get-attribute 'href (bits->attributes (links))))
    (define title (third (links)))
    (define icon (get-attribute 'src (bits->attributes ((query-selector (λ (e a c) (eq? e 'img)) item)))))
    (define description (second ((query-selector (λ (e a c) (eq? e 'p)) item))))
    (list '() title url icon description)))
