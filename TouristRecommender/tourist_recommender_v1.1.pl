% ==============================================================
% ENHANCED TOURIST DESTINATION RECOMMENDER SYSTEM
% Philippines Edition - Flexible & Interactive Version
% ==============================================================


% ==============================================================
% DESTINATION DATABASE (Enhanced with more attributes)
% ==============================================================

% destination(Name, Type, Region, Tags, Description, DailyCost, IdealDays)

% LUZON DESTINATIONS
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

destination(vigan, historical, ilocos,
    [heritage, unesco, culture, family_friendly, food],
    'Spanish colonial architecture with cobblestone streets and heritage houses',
    3000, 2).

destination(intramuros, historical, ncr,
    [historical, culture, family_friendly, educational],
    'Walled city with Spanish-era fortifications and museums',
    2000, 1).

destination(tagaytay, mountain, calabarzon,
    [mountain, cool_weather, scenic, food, family_friendly],
    'Scenic city with views of Taal Volcano and delicious local food',
    3500, 2).

destination(puerto_galera, beach, mimaropa,
    [beach, diving, snorkeling, nightlife, adventure],
    'Popular diving spot with beautiful coral reefs and white sand beaches',
    4000, 3).

destination(rizal, nature, calabarzon,
    [nature, hiking, waterfalls, adventure, camping],
    'Province with numerous hiking trails and beautiful waterfalls',
    2000, 2).

destination(bataan, historical, central_luzon,
    [historical, war_memorial, beaches, nature],
    'Historical WWII sites combined with beautiful coastal areas',
    2800, 2).

destination(zambales, beach, central_luzon,
    [beach, surfing, camping, island_hopping, adventure],
    'Surfing spots and crystal-clear waters perfect for island hopping',
    3000, 3).

% VISAYAS DESTINATIONS
destination(palawan, beach, palawan,
    [beach, nature, island_hopping, luxury, adventure, unesco],
    'Underground river, limestone cliffs, and pristine islands',
    5500, 5).

destination(bohol, nature, visayas,
    [nature, wildlife, adventure, family_friendly, geological],
    'Chocolate Hills, tarsiers, and Loboc River cruise',
    4000, 3).

destination(cebu, city_mix, visayas,
    [city, historical, beaches, food, shopping, adventure],
    'Queen City of the South with historical sites and beautiful beaches',
    4500, 4).

destination(bacolod, city_mix, visayas,
    [city, food, culture, historical, family_friendly],
    'City of Smiles known for delicious food and MassKara Festival',
    3500, 3).

destination(iloilo, city_mix, visayas,
    [city, food, historical, culture, family_friendly],
    'Rich cultural heritage and famous for delicious local cuisine',
    3200, 3).

destination(negros, nature, visayas,
    [nature, diving, waterfalls, adventure, luxury],
    'Diving paradise with beautiful marine life and waterfalls',
    4200, 4).

destination(siquijor, beach, visayas,
    [beach, mystical, waterfalls, healing, relaxation],
    'Mystical island known for waterfalls and traditional healers',
    3800, 3).

% MINDANAO DESTINATIONS
destination(davao, city_mix, mindanao,
    [city, nature, wildlife, adventure, family_friendly, food],
    'Home to Mount Apo, Philippine Eagle Center, and durian fruits',
    4500, 4).

destination(lake_sebu, nature, mindanao,
    [waterfalls, culture, adventure, indigenous, boating],
    'Seven waterfalls and rich Tboli indigenous culture',
    2300, 2).

destination(siargao, beach, mindanao,
    [beach, surfing, island_hopping, adventure, nightlife],
    'Surfing capital with beautiful lagoons and palm-lined beaches',
    5000, 4).

destination(camiguin, nature, mindanao,
    [nature, volcanoes, hot_springs, waterfalls, adventure],
    'Island born of fire with volcanoes, hot springs, and waterfalls',
    3500, 3).

destination(cotabato, nature, mindanao,
    [nature, waterfalls, adventure, indigenous, culture],
    'Home to Asik-Asik Falls and rich indigenous cultures',
    2500, 2).

destination(zamboanga, city_mix, mindanao,
    [city, historical, beaches, culture, food],
    'Asia''s Latin City with pink sand beaches and rich history',
    3800, 3).

destination(samal, beach, mindanao,
    [beach, island, resorts, family_friendly, adventure],
    'Island garden city with pristine beaches and resort facilities',
    4000, 3).

% ==============================================================
% SCORING SYSTEM (Enhanced)
% ==============================================================

% Rating out of 10 for different aspects

% LUZON DESTINATIONS
family_score(boracay, 8).
family_score(baguio, 9).
family_score(sagada, 6).
family_score(mount_pulag, 5).
family_score(mayon_volcano, 5).
family_score(vigan, 9).
family_score(intramuros, 10).
family_score(tagaytay, 8).
family_score(puerto_galera, 7).
family_score(rizal, 6).
family_score(bataan, 7).
family_score(zambales, 7).

% VISAYAS DESTINATIONS
family_score(palawan, 9).
family_score(bohol, 9).
family_score(cebu, 8).
family_score(bacolod, 8).
family_score(iloilo, 8).
family_score(negros, 7).
family_score(siquijor, 7).

% MINDANAO DESTINATIONS
family_score(davao, 9).
family_score(lake_sebu, 7).
family_score(siargao, 7).
family_score(camiguin, 8).
family_score(cotabato, 6).
family_score(zamboanga, 7).
family_score(samal, 9).

% Adventure Levels
adventure_level(boracay, 4).
adventure_level(baguio, 3).
adventure_level(sagada, 8).
adventure_level(mount_pulag, 9).
adventure_level(mayon_volcano, 10).
adventure_level(vigan, 2).
adventure_level(intramuros, 1).
adventure_level(tagaytay, 3).
adventure_level(puerto_galera, 7).
adventure_level(rizal, 8).
adventure_level(bataan, 5).
adventure_level(zambales, 7).
adventure_level(palawan, 7).
adventure_level(bohol, 6).
adventure_level(cebu, 5).
adventure_level(bacolod, 3).
adventure_level(iloilo, 4).
adventure_level(negros, 8).
adventure_level(siquijor, 5).
adventure_level(davao, 7).
adventure_level(lake_sebu, 8).
adventure_level(siargao, 9).
adventure_level(camiguin, 7).
adventure_level(cotabato, 8).
adventure_level(zamboanga, 5).
adventure_level(samal, 6).

% Relaxation Scores
relaxation_score(boracay, 9).
relaxation_score(baguio, 8).
relaxation_score(sagada, 7).
relaxation_score(mount_pulag, 4).
relaxation_score(mayon_volcano, 3).
relaxation_score(vigan, 9).
relaxation_score(intramuros, 9).
relaxation_score(tagaytay, 8).
relaxation_score(puerto_galera, 8).
relaxation_score(rizal, 6).
relaxation_score(bataan, 7).
relaxation_score(zambales, 8).
relaxation_score(palawan, 10).
relaxation_score(bohol, 8).
relaxation_score(cebu, 7).
relaxation_score(bacolod, 8).
relaxation_score(iloilo, 8).
relaxation_score(negros, 7).
relaxation_score(siquijor, 9).
relaxation_score(davao, 7).
relaxation_score(lake_sebu, 8).
relaxation_score(siargao, 6).
relaxation_score(camiguin, 8).
relaxation_score(cotabato, 6).
relaxation_score(zamboanga, 7).
relaxation_score(samal, 9).

% ==============================================================
% BEST TIME TO VISIT (Detailed seasons)
% ==============================================================

% best_time(Destination, [Months], Season, Weather)

% LUZON DESTINATIONS
best_time(boracay, [11,12,1,2,3,4], dry_season, 'Sunny and perfect for beach').
best_time(baguio, [1,2,3,4,11,12], cool_season, 'Cool and pleasant weather').
best_time(sagada, [1,2,3,4,11,12], dry_season, 'Clear skies for trekking').
best_time(mount_pulag, [1,2,3,4,5], dry_season, 'Best for sunrise viewing').
best_time(mayon_volcano, [1,2,3,4,5], dry_season, 'Safe for climbing').
best_time(vigan, [1,2,3,4,11,12], cool_season, 'Comfortable for walking tours').
best_time(intramuros, [1,2,3,4,11,12], cool_season, 'Pleasant for walking').
best_time(tagaytay, [1,2,3,4,11,12], cool_season, 'Cool weather with clear views').
best_time(puerto_galera, [1,2,3,4,12], dry_season, 'Calm seas for diving').
best_time(rizal, [1,2,3,4,11,12], dry_season, 'Ideal for hiking and outdoor activities').
best_time(bataan, [1,2,3,4,11,12], dry_season, 'Perfect for beach and historical tours').
best_time(zambales, [1,2,3,4,11,12], dry_season, 'Best for surfing and beach activities').

% VISAYAS DESTINATIONS
best_time(palawan, [1,2,3,4,12], dry_season, 'Calm seas for island hopping').
best_time(bohol, [1,2,3,4,12], dry_season, 'Ideal for tours').
best_time(cebu, [1,2,3,4,12], dry_season, 'Perfect for city tours and beach').
best_time(bacolod, [1,2,3,4,10,11,12], festival_season, 'MassKara Festival in October').
best_time(iloilo, [1,2,3,4,12], dry_season, 'Ideal for cultural tours').
best_time(negros, [1,2,3,4,12], dry_season, 'Best for diving and outdoor activities').
best_time(siquijor, [1,2,3,4,12], dry_season, 'Perfect for mystical tours').

% MINDANAO DESTINATIONS
best_time(davao, [1,2,3,4,12], dry_season, 'Less rainfall').
best_time(lake_sebu, [1,2,3,4,5,6], dry_season, 'Waterfalls at their best').
best_time(siargao, [3,4,5,6,7,8,9], surfing_season, 'Best waves for surfing').
best_time(camiguin, [1,2,3,4,12], dry_season, 'Ideal for island hopping').
best_time(cotabato, [1,2,3,4,12], dry_season, 'Best for waterfall viewing').
best_time(zamboanga, [1,2,3,4,12], dry_season, 'Perfect for pink sand beach').
best_time(samal, [1,2,3,4,12], dry_season, 'Ideal for beach vacation').

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
type_hierarchy(cultural, [cultural, historical, educational]).
type_hierarchy(food, [food, cultural, city_mix]).

% Enhanced food destination scoring - destinations known for food
food_destination(baguio, 9).  % Famous for strawberries, Good Shepherd products
food_destination(vigan, 8).   % Longganisa, empanada
food_destination(tagaytay, 8). % Bulalo, Bag of Beans
food_destination(cebu, 9).    % Lechon, dried mangoes
food_destination(bacolod, 10). % City of smiles, famous for food
food_destination(iloilo, 9).  % La Paz Batchoy, local delicacies
food_destination(davao, 8).   % Durian, local fruits
food_destination(zamboanga, 7). % Curacha, local seafood

% Get food score for any destination
get_food_score(Dest, Score) :-
    (food_destination(Dest, Score) -> true ; Score = 4).

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
    nl,
    write('Invalid choice! Please select a number between 1-5.'), nl,
    write('Please try again.'), nl, nl,
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
        '7. Food & Culinary',
        '8. Any type']
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
    validate_and_map_preference(Preference, Choice, Options).

validate_and_map_preference(Preference, Choice, Options) :-
    get_valid_range(Preference, MaxChoice),
    (number(Choice), Choice >= 1, Choice =< MaxChoice ->
        (map_preference(Preference, Choice, Value),
         asserta(user_preference(Preference, Value)))
    ;
        (nl,
         write('Invalid choice! Please enter a number between 1 and '), write(MaxChoice), write('.'), nl,
         write('Please try again.'), nl,
         ask_with_options(Preference, Options))
    ).

get_valid_range(type, 8).        % 8 options for destination type
get_valid_range(companions, 5).  % 5 options for companions
get_valid_range(activity_level, 5). % 5 options for activity level

write_option(Option) :-
    write(Option), nl.

map_preference(type, 1, beach).
map_preference(type, 2, nature).
map_preference(type, 3, historical).
map_preference(type, 4, city_mix).
map_preference(type, 5, adventure).
map_preference(type, 6, relaxation).
map_preference(type, 7, food).
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
    write('1. Budget (PHP 2,000 - 3,500)'), nl,
    write('2. Moderate (PHP 3,500 - 6,000)'), nl,
    write('3. Luxury (PHP 6,000 - 10,000)'), nl,
    write('4. Premium (PHP 10,000+)'), nl,    write('Enter choice: '),
    read(Choice),
    validate_budget_choice(Choice).

validate_budget_choice(Choice) :-
    (number(Choice), Choice >= 1, Choice =< 4 ->
        (map_budget(Choice, Range),
         asserta(user_preference(budget_range, Range)))
    ;
        (nl,
         write('Invalid choice! Please enter a number between 1 and 4.'), nl,
         write('Please try again.'), nl,
         ask_budget_range)
    ).

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
    validate_trip_duration(Days).

validate_trip_duration(Days) :-
    (number(Days), Days >= 1, Days =< 14 ->
        asserta(user_preference(duration, Days))
    ;
        (nl,
         write('Invalid input! Please enter a number between 1 and 14 days.'), nl,
         write('Please try again.'), nl,
         ask_trip_duration)
    ).

ask_month_preference :-
    nl,
    write('When do you plan to travel?'), nl,
    write('1. Specific month (enter 1-12)'), nl,
    write('2. Any month'), nl,
    write('Enter choice: '),
    read(Choice),
    validate_month_choice(Choice).

validate_month_choice(Choice) :-
    (Choice == 1 ->
        ask_specific_month
    ; Choice == 2 ->
        asserta(user_preference(month, any))
    ;
        (nl,
         write('Invalid choice! Please enter 1 or 2.'), nl,
         write('Please try again.'), nl,
         ask_month_preference)
    ).

ask_specific_month :-
    write('Enter month number (1-12): '),
    read(Month),
    validate_month_number(Month).

validate_month_number(Month) :-
    (number(Month), Month >= 1, Month =< 12 ->
        asserta(user_preference(month, Month))
    ;
        (nl,
         write('Invalid month! Please enter a number between 1 and 12.'), nl,
         write('(1=January, 2=February, ..., 12=December)'), nl,
         ask_specific_month)
    ).

% ==============================================================
% ENHANCED FILTERING SYSTEM
% ==============================================================

matches_all_preferences(Destination) :-
    destination(Destination, Type, _Region, _Tags, _, Cost, IdealDays),
    filter_type(Type), !,
    filter_companions(Destination), !,
    filter_activity(Destination), !,
    filter_budget(Cost), !,
    filter_duration(IdealDays), !,
    filter_month(Destination), !.

filter_type(_Type) :-
    user_preference(type, any), !.
filter_type(Type) :-
    user_preference(type, PrefType),
    (Type == PrefType -> true ;
     (type_hierarchy(Type, Hierarchy), member(PrefType, Hierarchy)) -> true ;
     (type_hierarchy(PrefType, Hierarchy), member(Type, Hierarchy))), !.

filter_companions(_Dest) :- 
    user_preference(companions, any), !.
filter_companions(Dest) :-
    user_preference(companions, Companions),
    (Companions == family -> 
        (family_score(Dest, Score), Score >= 7)
    ; Companions == couple ->
        (relaxation_score(Dest, Score), Score >= 7)
    ; Companions == friends ->
        (adventure_level(Dest, Score), Score >= 5)
    ; Companions == business ->
        destination(Dest, city_mix, _, _, _, _, _)
    ; true), !.

filter_activity(_Dest) :-
    user_preference(activity_level, any), !.
filter_activity(Dest) :-
    user_preference(activity_level, Activity),
    (Activity == relaxing ->
        (relaxation_score(Dest, Score), Score >= 7)
    ; Activity == moderate ->
        (adventure_level(Dest, Score), Score >= 3, Score =< 6)
    ; Activity == active ->
        (adventure_level(Dest, Score), Score >= 6, Score =< 8)
    ; Activity == extreme ->
        (adventure_level(Dest, Score), Score >= 8)
    ; Activity == mixed ->
        (adventure_level(Dest, A), relaxation_score(Dest, R), A >= 4, R >= 4)
    ; true), !.

filter_budget(_Cost) :-
    user_preference(budget_range, any), !.
filter_budget(Cost) :-
    user_preference(budget_range, Budget),
    (Budget == budget -> (Cost >= 2000, Cost =< 3500)
    ; Budget == moderate -> (Cost >= 2000, Cost =< 6000)
    ; Budget == luxury -> (Cost >= 4000, Cost =< 10000)
    ; Budget == premium -> Cost >= 8000
    ; true), !.

filter_duration(IdealDays) :-
    user_preference(duration, Days),
    Days >= 1,
    (Days >= IdealDays * 0.5, Days =< IdealDays * 1.5), !.

filter_month(_Dest) :-
    user_preference(month, any), !.
filter_month(Dest) :-
    user_preference(month, Month),
    best_time(Dest, Months, _, _),
    member(Month, Months), !.

% ==============================================================
% PERSONALIZED RECOMMENDATION ENGINE
% ==============================================================

generate_personalized_recommendations :-
    nl,
    write('==========================================='), nl,
    write('   PERSONALIZED RECOMMENDATIONS            '), nl,
    write('==========================================='), nl, nl,
    
    findall(D, matches_all_preferences(D), AllMatches),
    sort(AllMatches, Matches),  % This removes duplicates and sorts alphabetically
    (Matches = [] ->
        show_alternative_suggestions
    ;
        length(Matches, Count),
        format('Found ~w destinations matching your preferences:', [Count]), nl, nl,
        rank_and_display(Matches)
    ),
    
    ask_for_another_recommendation.

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
    destination(D, _Type, _Region, _Tags, _, Cost, IdealDays),
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
     best_time(D, Months, _, _), member(Month, Months) -> MonthScore = 10 ;
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
    write('   '), write(Desc), nl,
    format('   Daily Cost: PHP ~w | Ideal Stay: ~w days', [Cost, IdealDays]), nl,
    format('   Ratings: Family: ~w/10 | Adventure: ~w/10 | Relaxation: ~w/10', 
           [FScore, AScore, RScore]), nl,
    format('   Match Score: ~1f/10', [Score]), nl,
    
    % Show best time
    best_time(D, _Months, Season, Weather),
    format('   Best Time: ~w (~w)', [Season, Weather]), nl,
    
    % Show tags
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
    write('Here are some recommendations based on your preferences:'), nl, nl,
    
    findall(D, destination(D, _, _, _, _, _, _), AllDests),
    score_destinations_by_preference(AllDests, Scored),
    keysort(Scored, Sorted),
    reverse(Sorted, TopSorted),
    take_first(5, TopSorted, Top5),
    display_preference_based_destinations(Top5, 1),
    ask_for_another_recommendation.

score_destinations_by_preference([], []).
score_destinations_by_preference([D|Rest], [Score-D|ScoredRest]) :-
    calculate_preference_score(D, Score),
    score_destinations_by_preference(Rest, ScoredRest).

calculate_preference_score(D, Score) :-
    destination(D, Type, _, Tags, _, Cost, IdealDays),
    user_preference(type, PrefType),
    user_preference(companions, Companions),
    user_preference(activity_level, Activity),
    user_preference(budget_range, Budget),
    user_preference(duration, Days),
      % Type score (40% weight)
    (PrefType == any -> TypeScore = 5 ;
     calculate_type_score(D, Type, PrefType, TypeScore)),
    
    % Companion score (25% weight)
    (Companions == any -> CompScore = 5 ;
     calculate_companion_score(D, Companions, CompScore)),
    
    % Activity score (20% weight)
    (Activity == any -> ActScore = 5 ;
     calculate_activity_score(D, Activity, ActScore)),
    
    % Budget score (10% weight)
    (Budget == any -> BudScore = 5 ;
     calculate_budget_score(Cost, Budget, BudScore)),
    
    % Duration score (5% weight)
    DurScore is max(0, 10 - abs(Days - IdealDays)),
    
    % Calculate weighted total
    Score is TypeScore * 0.4 + CompScore * 0.25 + ActScore * 0.2 + BudScore * 0.1 + DurScore * 0.05.

calculate_type_score(Dest, Type, PrefType, Score) :-
    (Type == PrefType -> Score = 10 ;
     PrefType == food -> 
        get_food_score(Dest, FoodScore),
        (destination(Dest, _, _, Tags, _, _, _), member(food, Tags) ->
            Score is min(10, FoodScore + 1) ; 
            Score is min(10, FoodScore)
        ) ;
     type_hierarchy(Type, Hierarchy), member(PrefType, Hierarchy) -> Score = 8 ;
     type_hierarchy(PrefType, Hierarchy), member(Type, Hierarchy) -> Score = 8 ;
     Score = 2).

calculate_companion_score(D, Companions, Score) :-
    (Companions == family ->
        family_score(D, FamScore), Score is FamScore
    ; Companions == couple ->
        relaxation_score(D, RelScore), Score is RelScore
    ; Companions == friends ->
        adventure_level(D, AdvScore), Score is AdvScore
    ; Companions == business ->
        (destination(D, city_mix, _, _, _, _, _) -> Score = 9 ; Score = 3)
    ; Score = 5).

calculate_activity_score(D, Activity, Score) :-
    (Activity == relaxing ->
        relaxation_score(D, Score)
    ; Activity == extreme ->
        adventure_level(D, Score)
    ; Activity == moderate ->
        (adventure_level(D, A), A >= 3, A =< 6 -> Score = 8 ; Score = 4)
    ; Activity == active ->
        (adventure_level(D, A), A >= 6, A =< 8 -> Score = 8 ; Score = 4)
    ; Activity == mixed ->
        (adventure_level(D, A), relaxation_score(D, R), 
         A >= 4, R >= 4 -> Score = 9 ; Score = 5)
    ; Score = 5).

calculate_budget_score(Cost, Budget, Score) :-
    (Budget == budget ->
        (Cost >= 2000, Cost =< 3500 -> Score = 10 ; 
         Cost < 2000 -> Score = 0 ;  % Penalize destinations below minimum
         Score is max(0, 10 - (Cost - 3500)/400))
    ; Budget == moderate ->
        (Cost >= 2000, Cost =< 6000 -> Score = 10 ; 
         Cost < 2000 -> Score = 0 ;  % Penalize destinations below minimum
         Score is max(0, 8 - abs(Cost - 4000)/600))
    ; Budget == luxury ->
        (Cost >= 4000, Cost =< 10000 -> Score = 10 ; 
         Score is max(0, 8 - abs(Cost - 7000)/1000))
    ; Budget == premium ->
        (Cost >= 8000 -> Score = 10 ; Score is max(0, 6 - (8000 - Cost)/1000))
    ; Score = 5).

display_preference_based_destinations([], _).
display_preference_based_destinations([Score-D|Rest], Rank) :-
    destination(D, Type, Region, Tags, Desc, Cost, IdealDays),
    family_score(D, FScore),
    adventure_level(D, AScore),
    relaxation_score(D, RScore),
    
    format('~w. ~w (~w)', [Rank, D, Type]), nl,
    write('   '), write(Desc), nl,
    format('   PHP ~w per day | ~w days ideal | Region: ~w', [Cost, IdealDays, Region]), nl,
    format('   Ratings: Family: ~w/10 | Adventure: ~w/10 | Relaxation: ~w/10', 
           [FScore, AScore, RScore]), nl,
    format('   Match Score: ~1f/10', [Score]), nl,
    write('   Features: '), write_tags(Tags), nl, nl,
    
    NextRank is Rank + 1,
    display_preference_based_destinations(Rest, NextRank).

ask_for_another_recommendation :-
    nl,
    write('----------------------------------------'), nl,
    write('Do you want another recommendation again? (yes/no): '),
    read(Choice),
    handle_recommendation_choice(Choice).

handle_recommendation_choice(yes) :-
    conversational_recommender.

handle_recommendation_choice(y) :-
    conversational_recommender.

handle_recommendation_choice(no) :-
    main_menu.

handle_recommendation_choice(n) :-
    main_menu.

handle_recommendation_choice(_) :-
    write('Please enter yes or no.'), nl,
    ask_for_another_recommendation.

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
    write('4. Show all'), nl,    write('Enter choice: '),
    read(FilterChoice),
    validate_browse_choice(FilterChoice).

validate_browse_choice(FilterChoice) :-
    (number(FilterChoice), FilterChoice >= 1, FilterChoice =< 4 ->
        (FilterChoice == 1 -> browse_by_region ;
         FilterChoice == 2 -> browse_by_budget ;
         FilterChoice == 3 -> browse_by_activity ;
         display_all_destinations)
    ;
        (nl,
         write('Invalid choice! Please enter a number between 1 and 4.'), nl,
         write('Please try again.'), nl, nl,
         browse_all_destinations)
    ).

browse_by_region :-
    nl,
    write('Select region:'), nl,
    write('1. Luzon'), nl,
    write('2. Visayas'), nl,
    write('3. Mindanao'), nl,
    write('4. Palawan (separate region)'), nl,
    write('5. NCR (Metro Manila)'), nl,    write('Enter choice: '),
    read(RegionChoice),
    validate_region_choice(RegionChoice).

validate_region_choice(RegionChoice) :-
    (number(RegionChoice), RegionChoice >= 1, RegionChoice =< 5 ->
        (map_region_choice(RegionChoice, Region),
         display_destinations_by_region(Region))
    ;
        (nl,
         write('Invalid choice! Please enter a number between 1 and 5.'), nl,
         write('Please try again.'), nl,
         browse_by_region)
    ).

map_region_choice(1, luzon).
map_region_choice(2, visayas).
map_region_choice(3, mindanao).
map_region_choice(4, palawan).
map_region_choice(5, ncr).
map_region_choice(_, any).

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
    write('1. Budget (PHP 2,000-3,500)'), nl,
    write('2. Moderate (PHP 3,500-6,000)'), nl,
    write('3. Luxury (PHP 6,000+)'), nl,write('Enter choice: '),
    read(BudgetChoice),
    validate_budget_browse_choice(BudgetChoice).

validate_budget_browse_choice(BudgetChoice) :-
    (number(BudgetChoice), BudgetChoice >= 1, BudgetChoice =< 3 ->
        (BudgetChoice == 1 -> findall(D, (destination(D, _, _, _, _, Cost, _), Cost >= 2000, Cost =< 3500), Dests) ;
         BudgetChoice == 2 -> findall(D, (destination(D, _, _, _, _, Cost, _), Cost >= 3500, Cost =< 6000), Dests) ;
         BudgetChoice == 3 -> findall(D, (destination(D, _, _, _, _, Cost, _), Cost >= 6000), Dests)),
        display_destination_list(Dests, 1)
    ;
        (nl,
         write('Invalid choice! Please enter a number between 1 and 3.'), nl,
         write('Please try again.'), nl,
         browse_by_budget)
    ).

browse_by_activity :-
    nl,
    write('Select activity level:'), nl,
    write('1. Relaxing (Adventure score < 4)'), nl,
    write('2. Moderate (Adventure score 4-7)'), nl,
    write('3. Extreme (Adventure score > 7)'), nl,    write('Enter choice: '),
    read(ActChoice),
    validate_activity_browse_choice(ActChoice).

validate_activity_browse_choice(ActChoice) :-
    (number(ActChoice), ActChoice >= 1, ActChoice =< 3 ->
        (ActChoice == 1 -> findall(D, (adventure_level(D, Score), Score < 4), Dests) ;
         ActChoice == 2 -> findall(D, (adventure_level(D, Score), Score >= 4, Score =< 7), Dests) ;
         ActChoice == 3 -> findall(D, (adventure_level(D, Score), Score > 7), Dests)),
        display_destination_list(Dests, 1)
    ;
        (nl,
         write('Invalid choice! Please enter a number between 1 and 3.'), nl,
         write('Please try again.'), nl,
         browse_by_activity)
    ).

display_all_destinations :-
    findall(D, destination(D, _, _, _, _, _, _), Dests),
    display_destination_list(Dests, 1).

display_destination_list([], _).
display_destination_list([D|Rest], N) :-
    destination(D, Type, Region, _, Desc, Cost, IdealDays),
    format('~w. ~w (~w, ~w)', [N, D, Type, Region]), nl,
    write('   '), write(Desc), nl,
    format('   PHP ~w per day | ~w days ideal', [Cost, IdealDays]), nl, nl,
    NextN is N + 1,
    display_destination_list(Rest, NextN).

% ==============================================================
% SEARCH FUNCTIONALITY (UPDATED - REMOVED KEYWORD SEARCH)
% ==============================================================

search_by_criteria :-
    nl,
    write('==========================================='), nl,
    write('       ADVANCED SEARCH                     '), nl,
    write('==========================================='), nl, nl,
    
    write('Search by:'), nl,
    write('1. Specific tag'), nl,
    write('2. Maximum budget'), nl,
    write('3. Minimum family score'), nl,
    write('4. Return to main menu'), nl,
    write('Enter choice: '),
    read(SearchChoice),
      (SearchChoice == 1 -> search_by_tag ;
     SearchChoice == 2 -> search_by_max_budget ;
     SearchChoice == 3 -> search_by_min_family_score ;
     SearchChoice == 4 -> main_menu ;
     (nl, write('Invalid choice! Please select a number between 1-4.'), nl, 
      write('Please try again.'), nl, nl, search_by_criteria)).

search_by_tag :-
    nl,
    write('Available tags:'), nl,
    write('beach, mountain, nature, historical, culture, adventure,'), nl,
    write('family_friendly, luxury, hiking, water_sports, food, shopping,'), nl,
    write('diving, surfing, camping, island_hopping, spiritual, educational'), nl,
    write('Enter tag: '),
    read(Tag),
    findall(D, (destination(D, _, _, Tags, _, _, _), member(Tag, Tags)), Results),
    display_search_results(Results).

search_by_max_budget :-
    nl,
    write('Enter maximum daily budget (PHP): '),
    read(MaxBudget),
    validate_budget(MaxBudget).

validate_budget(MaxBudget) :-
    (MaxBudget < 2000 ->
        (nl,
         write('Budget too low! Minimum budget requirement is PHP 2,000.'), nl,
         write('This ensures you can afford basic accommodation, food, and activities.'), nl,
         write('Please enter a budget of at least PHP 2,000.'), nl, nl,
         search_by_max_budget)
    ;
        (findall(D, (destination(D, _, _, _, _, Cost, _), Cost =< MaxBudget), Results),
         display_search_results(Results))
    ).

search_by_min_family_score :-
    nl,
    write('Enter minimum family score (1-10): '),
    read(MinScore),
    validate_family_score(MinScore).

validate_family_score(MinScore) :-
    (number(MinScore), MinScore >= 1, MinScore =< 10 ->
        (findall(D, (family_score(D, Score), Score >= MinScore), Results),
         display_search_results(Results))
    ;
        (nl,
         write('Invalid input! Please enter a number between 1 and 10.'), nl,
         write('Family score represents how suitable a destination is for families.'), nl, nl,
         search_by_min_family_score)
    ).

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
    write('• Purchase travel insurance'), nl, nl,
    
    write('Regional Highlights:'), nl,
    write('• Luzon: Historical sites, mountain resorts, and vibrant cities'), nl,
    write('• Visayas: Beautiful beaches, diving spots, and cultural festivals'), nl,
    write('• Mindanao: Adventure destinations, indigenous cultures, and surfing'), nl.

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
    write('Description: '), write(Desc), nl, nl,
    format('Daily Cost: PHP ~w', [Cost]), nl,
    format('Ideal Stay: ~w days', [IdealDays]), nl, nl,
    write('Ratings:'), nl,
    format('  Family-friendly: ~w/10', [FScore]), nl,
    format('  Adventure level: ~w/10', [AScore]), nl,
    format('  Relaxation: ~w/10', [RScore]), nl, nl,
    write('Best Time to Visit: '), write(Season), nl,
    write('  ('), write(Weather), write(')'), nl,
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
    write('       '), write(Dest), write(' ITINERARY'), nl,
    write('==========================================='), nl, nl,
    
    format('Suggested ~w-day itinerary for ~w:', [Days, Dest]), nl, nl,
    
    (Dest == boracay -> boracay_itinerary(Days) ;
     Dest == baguio -> baguio_itinerary(Days) ;
     Dest == palawan -> palawan_itinerary(Days) ;
     Dest == vigan -> vigan_itinerary(Days) ;
     Dest == cebu -> cebu_itinerary(Days) ;
     Dest == lake_sebu -> lake_sebu_itinerary(Days) ;
     Dest == siargao -> siargao_itinerary(Days) ;
     Dest == tagaytay -> tagaytay_itinerary(Days) ;
     Dest == puerto_galera -> puerto_galera_itinerary(Days) ;
     Dest == camiguin -> camiguin_itinerary(Days) ;
     Dest == bohol -> bohol_itinerary(Days) ;
     Dest == davao -> davao_itinerary(Days) ;
     Dest == sagada -> sagada_itinerary(Days) ;
     Dest == mayon_volcano -> mayon_itinerary(Days) ;
     Dest == intramuros -> intramuros_itinerary(Days) ;
     default_itinerary(Dest, Days)).  % FIXED: Changed to default_itinerary/2


lake_sebu_itinerary(2) :-
    write('Day 1: Arrival and Cultural Immersion'), nl,
    write('  • Arrive in Lake Sebu'), nl,
    write('  • Hotel check-in (lakeside resort)'), nl,
    write('  • Visit Tboli Cultural Village'), nl,
    write('  • Traditional Tboli weaving demonstration'), nl,
    write('  • Dinner with local Tboli cuisine'), nl, nl,
    
    write('Day 2: Waterfalls and Departure'), nl,
    write('  • Seven Falls zipline adventure'), nl,
    write('  • Visit 2-3 of the major waterfalls'), nl,
    write('  • Lake Sebu boat tour'), nl,
    write('  • Buy Tboli handicrafts as souvenirs'), nl,
    write('  • Departure'), nl.

lake_sebu_itinerary(3) :-
    write('Day 1: Arrival and Lake Exploration'), nl,
    write('  • Arrive in Lake Sebu'), nl,
    write('  • Check into lakeside resort'), nl,
    write('  • Lake Sebu boat tour'), nl,
    write('  • Visit floating restaurants'), nl,
    write('  • Traditional Tboli dinner'), nl, nl,
    
    write('Day 2: Waterfalls Adventure'), nl,
    write('  • Seven Falls zipline experience'), nl,
    write('  • Trek to major waterfalls'), nl,
    write('  • Swimming at the falls'), nl,
    write('  • Photography sessions'), nl, nl,
    
    write('Day 3: Cultural Experience and Departure'), nl,
    write('  • Tboli Cultural Village visit'), nl,
    write('  • Traditional weaving demonstration'), nl,
    write('  • Tnalak cloth shopping'), nl,
    write('  • Lunch with local specialties'), nl,
    write('  • Departure'), nl.

siargao_itinerary(3) :-
    write('Day 1: Arrival and Island Introduction'), nl,
    write('  • Arrive at Sayak Airport'), nl,
    write('  • Transfer to accommodation'), nl,
    write('  • Sunset at Cloud 9'), nl,
    write('  • Dinner at local restaurant'), nl, nl,
    
    write('Day 2: Island Hopping'), nl,
    write('  • Three Islands Tour (Guyam, Daku, Naked)'), nl,
    write('  • Snorkeling and swimming'), nl,
    write('  • Surfing lessons (optional)'), nl,
    write('  • Nightlife at General Luna'), nl, nl,
    
    write('Day 3: Land Tour and Departure'), nl,
    write('  • Visit Magpupungko Rock Pools'), nl,
    write('  • Coconut Road photography'), nl,
    write('  • Souvenir shopping'), nl,
    write('  • Departure'), nl.

siargao_itinerary(4) :-
    write('Day 1: Arrival and Surfing Introduction'), nl,
    write('  • Arrive in Siargao'), nl,
    write('  • Hotel check-in'), nl,
    write('  • Surfing lesson at Cloud 9'), nl,
    write('  • Sunset viewing'), nl, nl,
    
    write('Day 2: Island Hopping Adventure'), nl,
    write('  • Three Islands Tour'), nl,
    write('  • Snorkeling at coral gardens'), nl,
    write('  • Beach picnic'), nl,
    write('  • Evening relaxation'), nl, nl,
    
    write('Day 3: Land Exploration'), nl,
    write('  • Visit Sugba Lagoon'), nl,
    write('  • Magpupungko Rock Pools'), nl,
    write('  • Coconut Road tour'), nl,
    write('  • Local market visit'), nl, nl,
    
    write('Day 4: Free Time and Departure'), nl,
    write('  • Optional surfing session'), nl,
    write('  • Last-minute shopping'), nl,
    write('  • Spa treatment (optional)'), nl,
    write('  • Departure'), nl.

tagaytay_itinerary(2) :-
    write('Day 1: Arrival and Scenic Views'), nl,
    write('  • Arrive in Tagaytay'), nl,
    write('  • Hotel check-in with Taal Volcano view'), nl,
    write('  • Visit Picnic Grove or People''s Park'), nl,
    write('  • Sky Ranch amusement park'), nl,
    write('  • Dinner at Leslie''s or Bulalo Point'), nl, nl,
    
    write('Day 2: Food Trip and Departure'), nl,
    write('  • Visit Mushroom Burger'), nl,
    write('  • Good Shepherd for ube jam'), nl,
    write('  • Rowena''s for pasalubong'), nl,
    write('  • Sonya''s Garden lunch'), nl,
    write('  • Departure'), nl.

puerto_galera_itinerary(3) :-
    write('Day 1: Arrival and Beach Time'), nl,
    write('  • Arrive in Puerto Galera'), nl,
    write('  • Check into beachfront resort'), nl,
    write('  • White Beach relaxation'), nl,
    write('  • Sunset viewing'), nl,
    write('  • Dinner at beach restaurant'), nl, nl,
    
    write('Day 2: Island Hopping and Diving'), nl,
    write('  • Island hopping tour'), nl,
    write('  • Snorkeling at coral gardens'), nl,
    write('  • Visit Hidden Beach'), nl,
    write('  • Diving (optional for certified divers)'), nl, nl,
    
    write('Day 3: Exploration and Departure'), nl,
    write('  • Visit Tamaraw Falls'), nl,
    write('  • Mangyan Village cultural visit'), nl,
    write('  • Souvenir shopping'), nl,
    write('  • Departure'), nl.

camiguin_itinerary(3) :-
    write('Day 1: Arrival and Island Tour'), nl,
    write('  • Arrive in Camiguin'), nl,
    write('  • Hotel check-in'), nl,
    write('  • Visit Katibawasan Falls'), nl,
    write('  • Stop at Old Volcano'), nl,
    write('  • Sunset at White Island'), nl, nl,
    
    write('Day 2: Sunken Cemetery and Hot Springs'), nl,
    write('  • Visit Sunken Cemetery'), nl,
    write('  • Guiob Church ruins'), nl,
    write('  • Sto. Niño Cold Springs'), nl,
    write('  • Ardent Hot Springs at night'), nl, nl,
    
    write('Day 3: Beach and Departure'), nl,
    write('  • Mantigue Island snorkeling'), nl,
    write('  • Last-minute souvenir shopping'), nl,
    write('  • Departure'), nl.


bohol_itinerary(3) :-
    write('Day 1: Arrival and Countryside Tour'), nl,
    write('  • Arrive in Bohol'), nl,
    write('  • Hotel check-in'), nl,
    write('  • Chocolate Hills viewing'), nl,
    write('  • Tarsier Sanctuary'), nl,
    write('  • Loboc River cruise lunch'), nl, nl,
    
    write('Day 2: Beach and Island Hopping'), nl,
    write('  • Panglao Island beach time'), nl,
    write('  • Island hopping to Balicasag or Virgin Island'), nl,
    write('  • Snorkeling or diving'), nl,
    write('  • Alona Beach nightlife'), nl, nl,
    
    write('Day 3: Historical and Departure'), nl,
    write('  • Visit Baclayon Church'), nl,
    write('  • Blood Compact Shrine'), nl,
    write('  • Souvenir shopping'), nl,
    write('  • Departure'), nl.


davao_itinerary(4) :-
    write('Day 1: Arrival and City Tour'), nl,
    write('  • Arrive in Davao'), nl,
    write('  • Hotel check-in'), nl,
    write('  • Visit People''s Park'), nl,
    write('  • Davao Baywalk'), nl,
    write('  • Durian tasting'), nl, nl,
    
    write('Day 2: Nature and Wildlife'), nl,
    write('  • Philippine Eagle Center'), nl,
    write('  • Malagos Garden Resort'), nl,
    write('  • Crocodile Park'), nl,
    write('  • Dinner at Jack''s Ridge'), nl, nl,
    
    write('Day 3: Samal Island Day Trip'), nl,
    write('  • Ferry to Samal Island'), nl,
    write('  • Beach relaxation'), nl,
    write('  • Island hopping'), nl,
    write('  • Return to Davao'), nl, nl,
    
    write('Day 4: Market and Departure'), nl,
    write('  • Visit Bankerohan Market'), nl,
    write('  • Buy Davao fruits and souvenirs'), nl,
    write('  • Departure'), nl.

sagada_itinerary(3) :-
    write('Day 1: Arrival and Echo Valley'), nl,
    write('  • Arrive in Sagada'), nl,
    write('  • Check into lodge'), nl,
    write('  • Echo Valley and Hanging Coffins'), nl,
    write('  • St. Mary the Virgin Church'), nl,
    write('  • Dinner at local restaurant'), nl, nl,
    
    write('Day 2: Caving Adventure'), nl,
    write('  • Sumaguing Cave exploration'), nl,
    write('  • Lunch at Yogurt House'), nl,
    write('  • Bomod-ok Falls trek'), nl,
    write('  • Sunset at Kiltepan Viewpoint'), nl, nl,
    
    write('Day 3: Cultural and Departure'), nl,
    write('  • Sagada Weaving'), nl,
    write('  • Buy woven products'), nl,
    write('  • Orange picking (seasonal)'), nl,
    write('  • Departure'), nl.

mayon_itinerary(2) :-
    write('Day 1: Arrival and Volcano Views'), nl,
    write('  • Arrive in Legazpi'), nl,
    write('  • Hotel check-in with Mayon view'), nl,
    write('  • Cagsawa Ruins'), nl,
    write('  • Mayon Skyline Viewdeck'), nl,
    write('  • Lignon Hill Nature Park'), nl, nl,
    
    write('Day 2: Adventure and Departure'), nl,
    write('  • ATV ride to lava front'), nl,
    write('  • Visit Sumlang Lake'), nl,
    write('  • Buy pili nuts and souvenirs'), nl,
    write('  • Departure'), nl.

intramuros_itinerary(1) :-
    write('Day 1: Historical Tour'), nl,
    write('  • Arrive at Intramuros'), nl,
    write('  • Fort Santiago entrance'), nl,
    write('  • Rizal Shrine museum'), nl,
    write('  • Manila Cathedral'), nl,
    write('  • San Agustin Church and Museum'), nl,
    write('  • Casa Manila museum'), nl,
    write('  • Kalesa ride around walls'), nl,
    write('  • Lunch at Barbara''s or Ilustrado'), nl,
    write('  • Buy souvenirs'), nl,
    write('  • Departure'), nl.



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

cebu_itinerary(4) :-
    write('Day 1: Arrival and City Tour'), nl,
    write('  • Arrive at Mactan-Cebu Airport'), nl,
    write('  • Hotel check-in'), nl,
    write('  • Visit Magellan''s Cross'), nl,
    write('  • Basilica Minore del Santo Niño'), nl,
    write('  • Dinner at local restaurant'), nl, nl,
    
    write('Day 2: Historical and Cultural Sites'), nl,
    write('  • Fort San Pedro'), nl,
    write('  • Taoist Temple'), nl,
    write('  • Casa Gorordo Museum'), nl,
    write('  • Try Cebu Lechon'), nl, nl,
    
    write('Day 3: Beach and Island Hopping'), nl,
    write('  • Travel to Mactan Island'), nl,
    write('  • Island hopping tour'), nl,
    write('  • Snorkeling and diving'), nl,
    write('  • Beach relaxation'), nl, nl,
    
    write('Day 4: Nature and Departure'), nl,
    write('  • Visit Sirao Flower Garden'), nl,
    write('  • Tops Lookout'), nl,
    write('  • Souvenir shopping'), nl,
    write('  • Departure'), nl.

default_itinerary(Dest, Days) :-
    format('Day 1: Arrival and Orientation in ~w', [Dest]), nl,
    write('  • Arrive at destination'), nl,
    write('  • Hotel check-in'), nl,
    write('  • Explore local area'), nl,
    write('  • Try local cuisine'), nl, nl,
    
    (Days > 2 ->
        format('Days 2-~w: Main Activities in ~w', [Days-1, Dest]), nl,
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
    
    format('Day ~w: Departure from ~w', [Days, Dest]), nl,
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

% Helper for case conversion (kept for other functions)
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

% Utility predicate for taking first N elements
take_first(0, _, []) :- !.
take_first(_N, [], []) :- !. 
take_first(N, [H|T], [H|R]) :-
    N1 is N - 1,
    take_first(N1, T, R).


