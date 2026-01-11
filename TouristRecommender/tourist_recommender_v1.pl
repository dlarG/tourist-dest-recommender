% ==============================================================
% ENHANCED TOURIST DESTINATION RECOMMENDER SYSTEM
% Philippines Edition - Flexible & Interactive Version
% ==============================================================

% :- dynamic user_preference/2.

% ==============================================================
% DESTINATION DATABASE (Enhanced with more attributes)
% ==============================================================

% destination(Name, Type, Region, Tags, Description, DailyCost, IdealDays)

destination(boracay, beach, visayas,
    [beach, nightlife, water_sports, luxury, family_friendly],
    'Famous white sand beaches with vibrant nightlife and water activities',
    6000, 3).

destination(baguio, mountain, luzon,
    [mountain, cool_weather, food, culture, family_friendly, shopping],
    'Summer capital with pine trees, cool climate, and local delicacies',
    3500, 4).

destination(sagada, mountain, luzon,
    [mountain, caves, culture, hiking, adventure, spiritual],
    'Mountain town famous for hanging coffins, caves, and scenic treks',
    2500, 3).

destination(mount_pulag, nature, luzon,
    [mountain, hiking, sunrise, adventure, camping],
    'Highest peak in Luzon offering breathtaking sea of clouds',
    2500, 2).

destination(mayon_volcano, nature, bicol,
    [volcano, hiking, extreme, scenic, photography],
    'Perfect cone volcano with challenging climbs and amazing views',
    3000, 2).

destination(davao, city_mix, mindanao,
    [city, nature, wildlife, adventure, family_friendly, food],
    'Home to Mount Apo, Philippine Eagle Center, and durian fruits',
    4500, 4).

destination(lake_sebu, nature, mindanao,
    [waterfalls, culture, adventure, indigenous, boating],
    'Seven waterfalls and rich Tboli indigenous culture',
    2300, 2).

destination(vigan, historical, ilocos,
    [heritage, unesco, culture, family_friendly, food],
    'Spanish colonial architecture with cobblestone streets and heritage houses',
    3000, 2).

destination(palawan, beach, palawan,
    [beach, nature, island_hopping, luxury, adventure, unesco],
    'Underground river, limestone cliffs, and pristine islands',
    5500, 5).

destination(bohol, nature, visayas,
    [nature, wildlife, adventure, family_friendly, geological],
    'Chocolate Hills, tarsiers, and Loboc River cruise',
    4000, 3).

destination(siargao, beach, mindanao,
    [beach, surfing, island_hopping, adventure, nightlife],
    'Surfing capital with beautiful lagoons and palm-lined beaches',
    5000, 4).

destination(intramuros, historical, ncr,
    [historical, culture, family_friendly, educational],
    'Walled city with Spanish-era fortifications and museums',
    2000, 1).

% ==============================================================
% SCORING SYSTEM (Enhanced)
% ==============================================================

% Rating out of 10 for different aspects
family_score(boracay, 8).
family_score(baguio, 9).
family_score(sagada, 6).
family_score(mount_pulag, 5).
family_score(mayon_volcano, 5).
family_score(davao, 9).
family_score(lake_sebu, 7).
family_score(vigan, 9).
family_score(palawan, 9).
family_score(bohol, 9).
family_score(siargao, 7).
family_score(intramuros, 10).

adventure_level(boracay, 4).
adventure_level(baguio, 3).
adventure_level(sagada, 8).
adventure_level(mount_pulag, 9).
adventure_level(mayon_volcano, 10).
adventure_level(davao, 7).
adventure_level(lake_sebu, 8).
adventure_level(vigan, 2).
adventure_level(palawan, 7).
adventure_level(bohol, 6).
adventure_level(siargao, 9).
adventure_level(intramuros, 1).

relaxation_score(boracay, 9).
relaxation_score(baguio, 8).
relaxation_score(sagada, 7).
relaxation_score(mount_pulag, 4).
relaxation_score(mayon_volcano, 3).
relaxation_score(davao, 7).
relaxation_score(lake_sebu, 8).
relaxation_score(vigan, 9).
relaxation_score(palawan, 10).
relaxation_score(bohol, 8).
relaxation_score(siargao, 6).
relaxation_score(intramuros, 9).

% ==============================================================
% BEST TIME TO VISIT (Detailed seasons)
% ==============================================================

% best_time(Destination, [Months], Season, Weather)
best_time(boracay, [11,12,1,2,3,4], dry_season, 'Sunny and perfect for beach').
best_time(baguio, [1,2,3,4,11,12], cool_season, 'Cool and pleasant weather').
best_time(sagada, [1,2,3,4,11,12], dry_season, 'Clear skies for trekking').
best_time(mount_pulag, [1,2,3,4,5], dry_season, 'Best for sunrise viewing').
best_time(mayon_volcano, [1,2,3,4,5], dry_season, 'Safe for climbing').
best_time(davao, [1,2,3,4,12], dry_season, 'Less rainfall').
best_time(lake_sebu, [1,2,3,4,5,6], dry_season, 'Waterfalls at their best').
best_time(vigan, [1,2,3,4,11,12], cool_season, 'Comfortable for walking tours').
best_time(palawan, [1,2,3,4,12], dry_season, 'Calm seas for island hopping').
best_time(bohol, [1,2,3,4,12], dry_season, 'Ideal for tours').
best_time(siargao, [3,4,5,6,7,8,9], surfing_season, 'Best waves for surfing').
best_time(intramuros, [1,2,3,4,11,12], cool_season, 'Pleasant for walking').

% ==============================================================
% TYPE ONTOLOGY
% ==============================================================

type_hierarchy(mountain, [mountain, nature, adventure]).
type_hierarchy(beach, [beach, nature, relaxation]).
type_hierarchy(nature, [nature, adventure, relaxation]).
type_hierarchy(historical, [historical, cultural, educational]).
type_hierarchy(city_mix, [city, cultural, food, shopping]).
type_hierarchy(adventure, [adventure, nature, extreme]).
type_hierarchy(relaxation, [relaxation, beach, nature]).

% ==============================================================
% MAIN INTERFACE
% ==============================================================

start :-
    clear_screen,
    display_welcome,
    main_menu.

main_menu :-
    nl, nl,
    write('==========================================='), nl,
    write('              MAIN MENU                    '), nl,
    write('==========================================='), nl,
    write('1. Get destination recommendations        '), nl,
    write('2. Browse all destinations                '), nl,
    write('3. Search by specific criteria            '), nl,
    write('4. View travel tips                       '), nl,
    write('5. Exit                                   '), nl,
    write('==========================================='), nl, nl,
    write('Enter your choice (1-5): '),
    read(Choice),
    handle_menu_choice(Choice).

handle_menu_choice(1) :-
    clear_screen,
    conversational_recommender,
    main_menu.

handle_menu_choice(2) :-
    clear_screen,
    browse_all_destinations,
    main_menu.

handle_menu_choice(3) :-
    clear_screen,
    search_by_criteria,
    main_menu.

handle_menu_choice(4) :-
    clear_screen,
    show_travel_tips,
    main_menu.

handle_menu_choice(5) :-
    write('Thank you for using the Philippine Travel Recommender!'), nl,
    write('Safe travels and enjoy the Philippines!'), nl.

handle_menu_choice(_) :-
    write('Invalid choice. Please try again.'), nl,
    main_menu.

% ==============================================================
% CONVERSATIONAL RECOMMENDER (Enhanced)
% ==============================================================

conversational_recommender :-
    retractall(user_preference(_, _)),
    write('Let us find your perfect destination!'), nl,
    write('I will ask a few questions to personalize your recommendations.'), nl, nl,
    
    ask_with_options(type, [
        'What type of destination are you looking for?',
        '1. Beach & Islands',
        '2. Mountains & Nature',
        '3. Historical & Cultural',
        '4. City & Urban',
        '5. Adventure & Extreme',
        '6. Relaxation & Wellness',
        '7. Any type']
    ),
    
    ask_with_options(companions, [
        'Who are you traveling with?',
        '1. Family (with children)',
        '2. Couple/Romantic',
        '3. Solo Traveler',
        '4. Friends Group',
        '5. Business/Conference']
    ),
    
    ask_with_options(activity_level, [
        'What activity level do you prefer?',
        '1. Relaxing (beach, spa, leisure)',
        '2. Moderate (sightseeing, light hikes)',
        '3. Active (hiking, water sports)',
        '4. Extreme (climbing, adventure sports)',
        '5. Mixed (combination)']
    ),
    
    ask_budget_range,
    ask_trip_duration,
    ask_month_preference,
    
    nl, write('Processing your preferences...'), nl,
    generate_personalized_recommendations.

% ==============================================================
% FLEXIBLE QUESTION ASKING
% ==============================================================

ask_with_options(Preference, Options) :-
    nl,
    maplist(write_option, Options),
    write('Enter choice number: '),
    read(Choice),
    map_preference(Preference, Choice, Value),
    asserta(user_preference(Preference, Value)).

write_option(Option) :-
    write(Option), nl.

map_preference(type, 1, beach).
map_preference(type, 2, nature).
map_preference(type, 3, historical).
map_preference(type, 4, city_mix).
map_preference(type, 5, adventure).
map_preference(type, 6, relaxation).
map_preference(type, _, any).

map_preference(companions, 1, family).
map_preference(companions, 2, couple).
map_preference(companions, 3, solo).
map_preference(companions, 4, friends).
map_preference(companions, 5, business).
map_preference(companions, _, any).

map_preference(activity_level, 1, relaxing).
map_preference(activity_level, 2, moderate).
map_preference(activity_level, 3, active).
map_preference(activity_level, 4, extreme).
map_preference(activity_level, 5, mixed).
map_preference(activity_level, _, any).

ask_budget_range :-
    nl,
    write('What is your approximate budget per day?'), nl,
    write('1. Budget (PHP 1,000 - 2,500)'), nl,
    write('2. Moderate (PHP 2,500 - 5,000)'), nl,
    write('3. Luxury (PHP 5,000 - 10,000)'), nl,
    write('4. Premium (PHP 10,000+)'), nl,
    write('Enter choice: '),
    read(Choice),
    map_budget(Choice, Range),
    asserta(user_preference(budget_range, Range)).

map_budget(1, budget).
map_budget(2, moderate).
map_budget(3, luxury).
map_budget(4, premium).
map_budget(_, any).

ask_trip_duration :-
    nl,
    write('How many days do you plan to stay?'), nl,
    write('Enter number of days (1-14): '),
    read(Days),
    asserta(user_preference(duration, Days)).

ask_month_preference :-
    nl,
    write('When do you plan to travel?'), nl,
    write('1. Specific month (enter 1-12)'), nl,
    write('2. Any month'), nl,
    write('Enter choice: '),
    read(Choice),
    (Choice == 1 ->
        write('Enter month number (1-12): '),
        read(Month),
        asserta(user_preference(month, Month))
    ;
        asserta(user_preference(month, any))
    ).

% ==============================================================
% ENHANCED FILTERING SYSTEM
% ==============================================================

matches_all_preferences(Destination) :-
    destination(Destination, Type, _Region, _Tags, _, Cost, IdealDays),
    filter_type(Type),
    filter_companions(Destination),
    filter_activity(Destination),
    filter_budget(Cost),
    filter_duration(IdealDays),
    filter_month(Destination).

filter_type(_Type) :-
    user_preference(type, any), !.
filter_type(Type) :-
    user_preference(type, PrefType),
    (type_hierarchy(Type, Hierarchy), member(PrefType, Hierarchy) ; 
     type_hierarchy(PrefType, Hierarchy), member(Type, Hierarchy) ;
     Type == PrefType).

filter_companions(_Dest) :- 
    user_preference(companions, any), !.
filter_companions(Dest) :-
    user_preference(companions, family),
    family_score(Dest, Score),
    Score >= 7.
filter_companions(Dest) :-
    user_preference(companions, couple),
    relaxation_score(Dest, Score),
    Score >= 7.
filter_companions(Dest) :-
    user_preference(companions, friends),
    adventure_level(Dest, Score),
    Score >= 5.
filter_companions(Dest) :-
    user_preference(companions, business),
    destination(Dest, city_mix, _, _, _, _, _).

filter_activity(_Dest) :-
    user_preference(activity_level, any), !.
filter_activity(Dest) :-
    user_preference(activity_level, relaxing),
    relaxation_score(Dest, Score),
    Score >= 7.
filter_activity(Dest) :-
    user_preference(activity_level, moderate),
    adventure_level(Dest, Score),
    Score >= 3, Score =< 6.
filter_activity(Dest) :-
    user_preference(activity_level, active),
    adventure_level(Dest, Score),
    Score >= 6, Score =< 8.
filter_activity(Dest) :-
    user_preference(activity_level, extreme),
    adventure_level(Dest, Score),
    Score >= 8.
filter_activity(Dest) :-
    user_preference(activity_level, mixed),
    adventure_level(Dest, A),
    relaxation_score(Dest, R),
    (A >= 4, R >= 4).

filter_budget(_Cost) :-
    user_preference(budget_range, any), !.
filter_budget(Cost) :-
    user_preference(budget_range, budget),
    Cost =< 2500.
filter_budget(Cost) :-
    user_preference(budget_range, moderate),
    Cost >= 1500, Cost =< 5000.
filter_budget(Cost) :-
    user_preference(budget_range, luxury),
    Cost >= 4000, Cost =< 10000.
filter_budget(Cost) :-
    user_preference(budget_range, premium),
    Cost >= 8000.

filter_duration(IdealDays) :-
    user_preference(duration, Days),
    Days >= 1,
    (Days >= IdealDays * 0.5 ; Days =< IdealDays * 1.5).

filter_month(_Dest) :-
    user_preference(month, any), !.
filter_month(Dest) :-
    user_preference(month, Month),
    best_time(Dest, Months, _, _),
    member(Month, Months).

% ==============================================================
% PERSONALIZED RECOMMENDATION ENGINE
% ==============================================================

generate_personalized_recommendations :-
    nl,
    write('==========================================='), nl,
    write('   PERSONALIZED RECOMMENDATIONS            '), nl,
    write('==========================================='), nl, nl,
    
    findall(D, matches_all_preferences(D), Matches),
    
    (Matches = [] ->
        show_alternative_suggestions
    ;
        length(Matches, Count),
        format('Found ~w destinations matching your preferences:', [Count]), nl, nl,
        rank_and_display(Matches)
    ),
    
    show_additional_suggestions.

rank_and_display(Destinations) :-
    score_destinations(Destinations, Scored),
    keysort(Scored, Sorted),
    reverse(Sorted, Ranked),
    display_ranked_destinations(Ranked, 1).

score_destinations([], []).
score_destinations([D|Rest], [Score-D|ScoredRest]) :-
    calculate_match_score(D, Score),
    score_destinations(Rest, ScoredRest).

calculate_match_score(D, Score) :-
    destination(D, _Type, _Region, _Tags, _, Cost, IdealDays),  % Changed to underscores
    user_preference(budget_range, BudgetPref),
    user_preference(companions, Companions),
    user_preference(activity_level, Activity),
    user_preference(month, Month),
    
    % Base score components
    (Companions == family -> family_score(D, FScore), ! ; FScore = 5),
    (Activity == relaxing -> relaxation_score(D, RScore), ! ; 
     Activity == extreme -> adventure_level(D, AScore), FScore is AScore * 0.5, RScore = 5 ; 
     FScore = 5, RScore = 5),
    
    % Budget match score
    (BudgetPref == budget -> BScore is max(0, 10 - Cost/250) ;
     BudgetPref == moderate -> BScore is max(0, 10 - abs(Cost-3750)/375) ;
     BudgetPref == luxury -> BScore is max(0, Cost/1000) ;
     BScore = 5),
    
    % Duration match score
    user_preference(duration, Days),
    DurationScore is 10 - abs(Days - IdealDays),
    
    % Month match score
    (Month == any -> MonthScore = 5 ;
     best_time(D, Months, _, _), member(Month, Months) -> MonthScore = 10 ;  % Added underscore for Season and Weather
     MonthScore = 3),
    
    % Calculate total score
    Score is FScore * 0.3 + RScore * 0.2 + BScore * 0.2 + DurationScore * 0.2 + MonthScore * 0.1.

display_ranked_destinations([], _).
display_ranked_destinations([Score-D|Rest], Rank) :-
    destination(D, Type, Region, Tags, Desc, Cost, IdealDays),
    family_score(D, FScore),
    adventure_level(D, AScore),
    relaxation_score(D, RScore),
    
    format('~w. ~w', [Rank, D]), nl,
    format('   Type: ~w | Region: ~w', [Type, Region]), nl,
    write('   '), write(Desc), nl,  % FIXED: Changed format/1 to write/1
    format('   Daily Cost: PHP ~w | Ideal Stay: ~w days', [Cost, IdealDays]), nl,
    format('   Ratings: Family: ~w/10 | Adventure: ~w/10 | Relaxation: ~w/10', 
           [FScore, AScore, RScore]), nl,
    format('   Match Score: ~1f/10', [Score]), nl,
    
    % Show best time
    best_time(D, _Months, Season, Weather),  % Changed Months to _Months
    format('   Best Time: ~w (~w)', [Season, Weather]), nl,
    
    % Show tags - FIXED: Changed format/1 to write/1
    write('   Features: '),
    write_tags(Tags),
    nl, nl,
    
    NextRank is Rank + 1,
    display_ranked_destinations(Rest, NextRank).

write_tags([]).
write_tags([Tag]) :-
    format('~w', [Tag]), !.
write_tags([Tag|Rest]) :-
    format('~w, ', [Tag]),
    write_tags(Rest).

% ==============================================================
% ALTERNATIVE SUGGESTIONS
% ==============================================================

show_alternative_suggestions :-
    nl,
    write('No destinations match all your criteria perfectly.'), nl,
    write('Here are some popular alternatives you might like:'), nl, nl,
    
    findall(D, destination(D, _, _, _, _, _, _), AllDests),
    score_popularity(AllDests, Popular),
    keysort(Popular, Sorted),
    reverse(Sorted, TopSorted),
    take_first(3, TopSorted, Top3),
    display_top_destinations(Top3).

score_popularity([], []).
score_popularity([D|Rest], [Score-D|ScoredRest]) :-
    destination(D, _, _, _, _, Cost, _),
    family_score(D, F),
    adventure_level(D, A),
    relaxation_score(D, R),
    Score is (F + A + R) * 10 - Cost/100,
    score_popularity(Rest, ScoredRest).

take_first(0, _, []) :- !.
take_first(_N, [], []) :- !. 
take_first(N, [H|T], [H|R]) :-
    N1 is N - 1,
    take_first(N1, T, R).

display_top_destinations([]).
display_top_destinations([_Score-D|Rest]) :- 
    destination(D, Type, _, _, Desc, Cost, Days),
    format('~w (~w)', [D, Type]), nl,
    write('   '), write(Desc), nl,  % FIXED: Changed format/1 to write/1
    format('   PHP ~w per day | ~w days ideal', [Cost, Days]), nl, nl,
    display_top_destinations(Rest).

show_additional_suggestions :-
    nl,
    write('----------------------------------------'), nl,
    write('Additional Options:'), nl,
    write('1. View detailed information about a destination'), nl,
    write('2. Get travel itinerary suggestions'), nl,
    write('3. Return to main menu'), nl,
    write('Enter choice: '),
    read(Choice),
    handle_additional_choice(Choice).

handle_additional_choice(1) :-
    nl,
    write('Enter destination name: '),
    read(Dest),
    (destination(Dest, _, _, _, _, _, _) ->
        show_destination_details(Dest)
    ;
        write('Destination not found.'), nl
    ),
    show_additional_suggestions.

handle_additional_choice(2) :-
    nl,
    write('Enter destination name for itinerary: '),
    read(Dest),
    (destination(Dest, _, _, _, _, _, IdealDays) ->
        generate_itinerary(Dest, IdealDays)
    ;
        write('Destination not found.'), nl
    ),
    show_additional_suggestions.

handle_additional_choice(3).

% ==============================================================
% BROWSE FUNCTIONALITY
% ==============================================================

browse_all_destinations :-
    nl,
    write('==========================================='), nl,
    write('       ALL DESTINATIONS                    '), nl,
    write('==========================================='), nl, nl,
    
    write('Filter by:'), nl,
    write('1. Region'), nl,
    write('2. Budget Range'), nl,
    write('3. Activity Level'), nl,
    write('4. Show all'), nl,
    write('Enter choice: '),
    read(FilterChoice),
    
    (FilterChoice == 1 -> browse_by_region ;
     FilterChoice == 2 -> browse_by_budget ;
     FilterChoice == 3 -> browse_by_activity ;
     display_all_destinations).

browse_by_region :-
    nl,
    write('Select region:'), nl,
    write('1. Luzon'), nl,
    write('2. Visayas'), nl,
    write('3. Mindanao'), nl,
    write('4. Palawan'), nl,
    write('5. NCR (Metro Manila)'), nl,
    write('Enter choice: '),
    read(RegionChoice),
    map_region_choice(RegionChoice, Region),
    display_destinations_by_region(Region).

map_region_choice(1, luzon).
map_region_choice(2, visayas).
map_region_choice(3, mindanao).
map_region_choice(4, palawan).
map_region_choice(5, ncr).

display_destinations_by_region(Region) :-
    findall(D, (destination(D, _, R, _, _, _, _), R == Region), Dests),
    (Dests = [] ->
        write('No destinations found in this region.'), nl
    ;
        format('Destinations in ~w:', [Region]), nl, nl,
        display_destination_list(Dests, 1)
    ).

browse_by_budget :-
    nl,
    write('Select budget range per day:'), nl,
    write('1. Budget (Under PHP 2,500)'), nl,
    write('2. Moderate (PHP 2,500-5,000)'), nl,
    write('3. Luxury (PHP 5,000+)'), nl,
    write('Enter choice: '),
    read(BudgetChoice),
    (BudgetChoice == 1 -> findall(D, (destination(D, _, _, _, _, Cost, _), Cost =< 2500), Dests) ;
     BudgetChoice == 2 -> findall(D, (destination(D, _, _, _, _, Cost, _), Cost >= 2500, Cost =< 5000), Dests) ;
     BudgetChoice == 3 -> findall(D, (destination(D, _, _, _, _, Cost, _), Cost >= 5000), Dests)),
    display_destination_list(Dests, 1).

browse_by_activity :-
    nl,
    write('Select activity level:'), nl,
    write('1. Relaxing (Adventure score < 4)'), nl,
    write('2. Moderate (Adventure score 4-7)'), nl,
    write('3. Extreme (Adventure score > 7)'), nl,
    write('Enter choice: '),
    read(ActChoice),
    (ActChoice == 1 -> findall(D, (adventure_level(D, Score), Score < 4), Dests) ;
     ActChoice == 2 -> findall(D, (adventure_level(D, Score), Score >= 4, Score =< 7), Dests) ;
     ActChoice == 3 -> findall(D, (adventure_level(D, Score), Score > 7), Dests)),
    display_destination_list(Dests, 1).

display_all_destinations :-
    findall(D, destination(D, _, _, _, _, _, _), Dests),
    display_destination_list(Dests, 1).

display_destination_list([], _).
display_destination_list([D|Rest], N) :-
    destination(D, Type, Region, _, Desc, Cost, IdealDays),
    format('~w. ~w (~w, ~w)', [N, D, Type, Region]), nl,
    write('   '), write(Desc), nl,  % FIXED: Changed format/1 to write/1
    format('   PHP ~w per day | ~w days ideal', [Cost, IdealDays]), nl, nl,
    NextN is N + 1,
    display_destination_list(Rest, NextN).

% ==============================================================
% SEARCH FUNCTIONALITY
% ==============================================================

search_by_criteria :-
    nl,
    write('==========================================='), nl,
    write('       ADVANCED SEARCH                     '), nl,
    write('==========================================='), nl, nl,
    
    write('Search by:'), nl,
    write('1. Keyword in description'), nl,
    write('2. Specific tag'), nl,
    write('3. Maximum budget'), nl,
    write('4. Minimum family score'), nl,
    write('Enter choice: '),
    read(SearchChoice),
    
    (SearchChoice == 1 -> search_by_keyword ;
     SearchChoice == 2 -> search_by_tag ;
     SearchChoice == 3 -> search_by_max_budget ;
     SearchChoice == 4 -> search_by_min_family_score ;
     write('Invalid choice.'), nl).

% FIXED search_by_keyword: Simplified string handling
search_by_keyword :-
    nl,
    write('Enter keyword: '),
    read(Keyword),
    atom_string(Keyword, KeywordStr),  % Convert atom to string if needed
    findall(D, (destination(D, _, _, _, Desc, _, _),
                sub_string(Desc, _, _, _, KeywordStr)), Results),
    display_search_results(Results).

% Alternative: Case-insensitive search
search_by_keyword_ci :-
    nl,
    write('Enter keyword: '),
    read(Keyword),
    % Simple case-insensitive search by checking if keyword appears in description
    findall(D, (destination(D, _, _, _, Desc, _, _),
                (sub_string(Desc, _, _, _, Keyword) ; 
                 (upcase_atom(Keyword, Upper), sub_string(Desc, _, _, _, Upper)) ;
                 (downcase_atom(Keyword, Lower), sub_string(Desc, _, _, _, Lower)))), Results),
    display_search_results(Results).

search_by_tag :-
    nl,
    write('Available tags: beach, mountain, nature, historical, culture, adventure,'), nl,
    write('family_friendly, luxury, hiking, water_sports, food, shopping'), nl,
    write('Enter tag: '),
    read(Tag),
    findall(D, (destination(D, _, _, Tags, _, _, _), member(Tag, Tags)), Results),
    display_search_results(Results).

search_by_max_budget :-
    nl,
    write('Enter maximum daily budget (PHP): '),
    read(MaxBudget),
    findall(D, (destination(D, _, _, _, _, Cost, _), Cost =< MaxBudget), Results),
    display_search_results(Results).

search_by_min_family_score :-
    nl,
    write('Enter minimum family score (1-10): '),
    read(MinScore),
    findall(D, (family_score(D, Score), Score >= MinScore), Results),
    display_search_results(Results).

display_search_results([]) :-
    nl,
    write('No destinations found matching your criteria.'), nl.

display_search_results(Results) :-
    nl,
    format('Found ~w destinations:', [length(Results)]), nl, nl,
    display_destination_list(Results, 1).

% ==============================================================
% TRAVEL TIPS
% ==============================================================

show_travel_tips :-
    nl,
    write('==========================================='), nl,
    write('       TRAVEL TIPS & ADVICE                '), nl,
    write('==========================================='), nl, nl,
    
    write('General Tips:'), nl,
    write('• Dry season in the Philippines is typically November to April'), nl,
    write('• Always check weather advisories before traveling'), nl,
    write('• Respect local customs and traditions'), nl,
    write('• Try local cuisine for an authentic experience'), nl, nl,
    
    write('Budget Tips:'), nl,
    write('• Travel during off-peak seasons for better rates'), nl,
    write('• Consider local transportation options'), nl,
    write('• Book accommodations in advance'), nl,
    write('• Eat at local eateries for authentic and affordable food'), nl, nl,
    
    write('Safety Tips:'), nl,
    write('• Keep copies of important documents'), nl,
    write('• Stay hydrated and use sun protection'), nl,
    write('• Be aware of local emergency numbers'), nl,
    write('• Purchase travel insurance'), nl.

% ==============================================================
% DESTINATION DETAILS
% ==============================================================

show_destination_details(Dest) :-
    destination(Dest, Type, Region, Tags, Desc, Cost, IdealDays),
    family_score(Dest, FScore),
    adventure_level(Dest, AScore),
    relaxation_score(Dest, RScore),
    best_time(Dest, Months, Season, Weather),
    
    nl,
    write('==========================================='), nl,
    write('       DESTINATION DETAILS                 '), nl,
    write('==========================================='), nl, nl,
    
    format('~w', [Dest]), nl,
    write_line(40),
    format('Type: ~w | Region: ~w', [Type, Region]), nl, nl,
    write('Description: '), write(Desc), nl, nl,  % FIXED: Changed format/1 to write/1
    format('Daily Cost: PHP ~w', [Cost]), nl,
    format('Ideal Stay: ~w days', [IdealDays]), nl, nl,
    write('Ratings:'), nl,
    format('  Family-friendly: ~w/10', [FScore]), nl,
    format('  Adventure level: ~w/10', [AScore]), nl,
    format('  Relaxation: ~w/10', [RScore]), nl, nl,
    write('Best Time to Visit: '), write(Season), nl,  % FIXED: Changed format/1 to write/1
    write('  ('), write(Weather), write(')'), nl,  % FIXED: Changed format/1 to write/1
    write('  Recommended months: '),
    write_months(Months), nl, nl,
    write('Features: '), write_tags(Tags), nl, nl,
    
    % Calculate total trip cost
    (user_preference(duration, Days) -> true ; Days = IdealDays),
    TotalCost is Cost * Days,
    format('Estimated ~w-day trip cost: PHP ~w', [Days, TotalCost]), nl.

write_line(N) :-
    N > 0,
    write('='),
    N1 is N - 1,
    write_line(N1).
write_line(0) :- nl.

write_months([]).
write_months([M]) :-
    month_name(M, Name),
    format('~w', [Name]), !.
write_months([M|Rest]) :-
    month_name(M, Name),
    format('~w, ', [Name]),
    write_months(Rest).

month_name(1, 'January').
month_name(2, 'February').
month_name(3, 'March').
month_name(4, 'April').
month_name(5, 'May').
month_name(6, 'June').
month_name(7, 'July').
month_name(8, 'August').
month_name(9, 'September').
month_name(10, 'October').
month_name(11, 'November').
month_name(12, 'December').

% ==============================================================
% ITINERARY GENERATOR
% ==============================================================

generate_itinerary(Dest, Days) :-
    nl,
    write('==========================================='), nl,
    write('       '), write(Dest), write(' ITINERARY'), nl,  % FIXED: Changed format/1 to write/1
    write('==========================================='), nl, nl,
    
    format('Suggested ~w-day itinerary for ~w:', [Days, Dest]), nl, nl,
    
    (Dest == boracay -> boracay_itinerary(Days) ;
     Dest == baguio -> baguio_itinerary(Days) ;
     Dest == palawan -> palawan_itinerary(Days) ;
     Dest == vigan -> vigan_itinerary(Days) ;
     default_itinerary(Days)).

boracay_itinerary(3) :-
    write('Day 1: Arrival and White Beach'), nl,
    write('  • Arrive at Caticlan Airport'), nl,
    write('  • Transfer to hotel'), nl,
    write('  • Sunset at White Beach'), nl,
    write('  • Dinner at D Mall'), nl, nl,
    
    write('Day 2: Island Activities'), nl,
    write('  • Island hopping tour'), nl,
    write('  • Snorkeling and swimming'), nl,
    write('  • Visit Puka Shell Beach'), nl,
    write('  • Nightlife at Station 2'), nl, nl,
    
    write('Day 3: Relaxation and Departure'), nl,
    write('  • Morning spa treatment'), nl,
    write('  • Last-minute shopping'), nl,
    write('  • Departure'), nl.

baguio_itinerary(4) :-
    write('Day 1: Arrival and City Tour'), nl,
    write('  • Arrive in Baguio'), nl,
    write('  • Check into hotel'), nl,
    write('  • Visit Burnham Park'), nl,
    write('  • Dinner at Session Road'), nl, nl,
    
    write('Day 2: Historical Sites'), nl,
    write('  • Baguio Cathedral'), nl,
    write('  • Camp John Hay'), nl,
    write('  • The Mansion'), nl,
    write('  • Mines View Park'), nl, nl,
    
    write('Day 3: Market and Food'), nl,
    write('  • Baguio Public Market'), nl,
    write('  • Good Shepherd Convent'), nl,
    write('  • Strawberry picking (seasonal)'), nl,
    write('  • Try local cuisine'), nl, nl,
    
    write('Day 4: Nature and Departure'), nl,
    write('  • Botanical Garden'), nl,
    write('  • Wright Park'), nl,
    write('  • Souvenir shopping'), nl,
    write('  • Departure'), nl.

palawan_itinerary(5) :-
    write('Day 1: Arrival in Puerto Princesa'), nl,
    write('  • Arrive at Puerto Princesa Airport'), nl,
    write('  • Hotel check-in'), nl,
    write('  • City tour'), nl,
    write('  • Dinner at Ka Lui'), nl, nl,
    
    write('Day 2: Underground River'), nl,
    write('  • Travel to Sabang'), nl,
    write('  • Puerto Princesa Underground River tour'), nl,
    write('  • Monkey Trail and Ugong Rock'), nl,
    write('  • Return to Puerto Princesa'), nl, nl,
    
    write('Day 3: Honda Bay Island Hopping'), nl,
    write('  • Honda Bay island hopping'), nl,
    write('  • Snorkeling at Starfish Island'), nl,
    write('  • Cowrie Island'), nl,
    write('  • Luli Island'), nl, nl,
    
    write('Day 4: El Nido or Coron option'), nl,
    write('  • Option 1: Fly to El Nido'), nl,
    write('  • Option 2: Fly to Coron'), nl,
    write('  • Island hopping tours'), nl,
    write('  • Kayaking and snorkeling'), nl, nl,
    
    write('Day 5: Departure'), nl,
    write('  • Last-minute shopping'), nl,
    write('  • Buy pasalubong'), nl,
    write('  • Departure'), nl.

vigan_itinerary(2) :-
    write('Day 1: Heritage Walk'), nl,
    write('  • Arrive in Vigan'), nl,
    write('  • Hotel check-in (heritage house)'), nl,
    write('  • Calle Crisologo walking tour'), nl,
    write('  • Kalesa ride'), nl,
    write('  • Dinner at Cafe Leona'), nl, nl,
    
    write('Day 2: Cultural Experience'), nl,
    write('  • Visit Baluarte Zoo'), nl,
    write('  • Pagburnayan pottery making'), nl,
    write('  • Hidden Garden'), nl,
    write('  • Syquia Mansion Museum'), nl,
    write('  • Buy Vigan longganisa and empanada'), nl,
    write('  • Departure'), nl.

default_itinerary(Days) :-
    format('Day 1: Arrival and Orientation'), nl,
    write('  • Arrive at destination'), nl,
    write('  • Hotel check-in'), nl,
    write('  • Explore local area'), nl,
    write('  • Try local cuisine'), nl, nl,
    
    (Days > 2 ->
        format('Days 2-~w: Main Activities', [Days-1]), nl,
        write('  • Visit major attractions'), nl,
        write('  • Experience local culture'), nl,
        write('  • Outdoor activities'), nl,
        write('  • Shopping and dining'), nl, nl
    ;
        write('Day 2: Main Activities'), nl,
        write('  • Visit major attractions'), nl,
        write('  • Experience local culture'), nl,
        write('  • Shopping for souvenirs'), nl, nl
    ),
    
    format('Day ~w: Departure', [Days]), nl,
    write('  • Last-minute activities'), nl,
    write('  • Souvenir shopping'), nl,
    write('  • Depart for home'), nl.

% ==============================================================
% UTILITY PREDICATES
% ==============================================================

clear_screen :-
    write('\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n').

display_welcome :-
    write('==========================================='), nl,
    write('  WELCOME TO PHILIPPINE TRAVEL PLANNER    '), nl,
    write('        Discover Paradise Islands          '), nl,
    write('==========================================='), nl, nl,
    write('Your personal guide to the best destinations'), nl,
    write('in the beautiful Philippines!'), nl, nl.

% Helper for case conversion
upcase_atom(Atom, Upper) :-
    atom_codes(Atom, Codes),
    maplist(to_upper, Codes, UpperCodes),
    atom_codes(Upper, UpperCodes).

to_upper(Code, UpperCode) :-
    (Code >= 97, Code =< 122 ->
        UpperCode is Code - 32
    ;
        UpperCode = Code
    ).

downcase_atom(Atom, Lower) :-
    atom_codes(Atom, Codes),
    maplist(to_lower, Codes, LowerCodes),
    atom_codes(Lower, LowerCodes).

to_lower(Code, LowerCode) :-
    (Code >= 65, Code =< 90 ->
        LowerCode is Code + 32
    ;
        LowerCode = Code
    ).

% Helper for substring search (simplified)
sub_string(String, Before, Length, After, SubString) :-
    sub_string(String, Before, Length, After, SubString).

% ==============================================================
% START THE APPLICATION
% ==============================================================

% Run with: ?- start.