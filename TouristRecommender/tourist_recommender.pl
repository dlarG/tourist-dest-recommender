% Format: destination(Name, Type, Tags, Description, SingleDayCost)
destination(boracay, beach, [beach, party, water_sports, luxury, nightlife], 
    'Famous white sand beach with vibrant nightlife and water activities', 6000).
destination(palawan, nature, [beach, island_hopping, nature, unesco, diving, adventure], 
    'Home to Puerto Princesa Underground River, El Nido lagoons, and Coron wreck diving', 5500).
destination(cebu, city_mix, [historical, beaches, city, food, shopping, family], 
    'Historical sites (Magellans Cross), beautiful beaches, and delicious lechon', 4500).
destination(bohol, nature, [nature, historical, family, wildlife, river], 
    'Chocolate Hills, tarsier sanctuary, and Loboc River cruise', 4000).
destination(baguio, mountain, [mountain, cool_weather, family, arts, shopping, food], 
    'Summer capital with cool climate, pine trees, and strawberry farms', 3500).
destination(siargao, beach, [beach, surfing, nature, adventure, backpacker], 
    'Surfing capital with laid-back island vibe and Cloud 9 surf break', 5000).
destination(vigan, historical, [historical, unesco, culture, family, heritage], 
    'Spanish colonial architecture, horse-drawn carriages, and heritage sites', 3000).
destination(batanes, nature, [nature, remote, culture, scenic, photography, adventure], 
    'Northernmost islands with unique culture, rolling hills, and stone houses', 6500).
destination(davao, city_mix, [city, nature, family, adventure, wildlife, fruit], 
    'Home to Mount Apo, Philippine Eagle Center, and durian fruit', 4500).
destination(manila, city, [historical, city, shopping, food, nightlife, museums], 
    'Capital city with historical Intramuros, modern malls, and museums', 5000).
destination(coron, nature, [beach, island_hopping, diving, nature, adventure, luxury], 
    'World-class wreck diving, Kayangan Lake, and limestone cliffs', 6000).
destination(sagada, mountain, [mountain, culture, adventure, nature, caves, sunrise], 
    'Mountain town with hanging coffins, caves, waterfalls, and cool climate', 2500).
destination(banaue, nature, [nature, culture, unesco, scenic, trekking, photography], 
    'Ancient rice terraces, indigenous culture, and scenic views', 2000).
destination(zambales, beach, [beach, surfing, camping, budget, nature, adventure], 
    'Beaches, surf spots, and camping sites near Manila', 1800).
destination(iloilo, city_mix, [food, historical, culture, city, island, heritage], 
    'Famous for La Paz Batchoy, historical churches, and islands', 3500).


% Nature destinations with high adventure level
destination(mount_apo, nature, [nature, mountain_climbing, extreme, adventure, wildlife, hiking], 
    'Highest peak in the Philippines with challenging climbs and unique biodiversity', 3500).
destination(tubbataha_reef, nature, [nature, diving, unesco, marine_life, extreme, adventure], 
    'UNESCO World Heritage site with world-class diving and rich marine biodiversity', 7000).
destination(mayon_volcano, nature, [nature, volcano, hiking, adventure, scenic, extreme], 
    'Perfect cone-shaped volcano with challenging climbs and stunning views', 3000).
destination(apo_reef, nature, [nature, diving, snorkeling, marine_life, adventure, remote], 
    'Second largest contiguous coral reef system in the world', 5500).


% Nature destinations good for June travel
destination(elnido, nature, [beach, nature, island_hopping, lagoon, adventure, scenic], 
    'Paradise of limestone cliffs, hidden lagoons, and pristine beaches', 5000).
destination(apo_island, nature, [nature, diving, snorkeling, marine_life, family, adventure], 
    'Marine sanctuary with sea turtle encounters and diverse marine life', 4000).
destination(mount_pulag, nature, [nature, mountain, hiking, sunrise, adventure, scenic], 
    'Sea of Clouds phenomenon and challenging high-altitude trek', 2500).
destination(taal_volcano, nature, [nature, volcano, hiking, adventure, scenic, historical], 
    'Smallest active volcano with horseback riding and crater lake view', 2800).

% More family-friendly nature destinations
destination(puerto_princesa, nature, [nature, underground_river, unesco, family, adventure, wildlife], 
    'Home to the Underground River, wildlife safari, and firefly watching', 4200).
destination(tagaytay, nature, [nature, cool_weather, family, scenic, food, adventure], 
    'Scenic views of Taal Volcano, cool climate, and gourmet restaurants', 3200).
destination(lake_sebu, nature, [nature, lake, culture, family, adventure, waterfalls], 
    'Seven waterfalls, Tboli culture, and zipline adventure', 2300).
destination(camiguin, nature, [nature, island, volcano, waterfalls, springs, family], 
    'Island born of fire with volcanoes, hot springs, and white sand beaches', 3800).


% COST BREAKDOWN PER DESTINATION (in PHP per day)
% Format: cost_breakdown(Destination, Accommodation, Food, Activities, Transportation, Miscellaneous)
cost_breakdown(boracay, 2500, 1500, 1200, 500, 300).
cost_breakdown(palawan, 2200, 1200, 1500, 400, 200).
cost_breakdown(cebu, 1800, 1000, 800, 700, 200).
cost_breakdown(bohol, 1500, 800, 700, 800, 200).
cost_breakdown(baguio, 1200, 600, 500, 600, 200).
cost_breakdown(siargao, 2000, 1000, 1200, 600, 200).
cost_breakdown(vigan, 1000, 500, 400, 800, 300).
cost_breakdown(batanes, 2500, 1200, 1000, 1200, 600).
cost_breakdown(davao, 1800, 1000, 800, 700, 200).
cost_breakdown(manila, 2000, 1500, 800, 300, 400).
cost_breakdown(coron, 2200, 1200, 1800, 600, 200).
cost_breakdown(sagada, 800, 400, 600, 400, 300).
cost_breakdown(banaue, 600, 400, 400, 400, 200).
cost_breakdown(zambales, 600, 400, 300, 400, 100).
cost_breakdown(iloilo, 1200, 800, 600, 600, 200).

cost_breakdown(mount_apo, 1000, 800, 1000, 500, 200).
cost_breakdown(tubbataha_reef, 3000, 1500, 2000, 400, 100).
cost_breakdown(mayon_volcano, 1200, 700, 800, 200, 100).
cost_breakdown(apo_reef, 2500, 1200, 1500, 500, 200).
cost_breakdown(elnido, 2000, 1200, 1400, 300, 100).
cost_breakdown(apo_island, 1500, 800, 1200, 400, 100).
cost_breakdown(mount_pulag, 800, 600, 800, 300, 100).
cost_breakdown(taal_volcano, 1200, 700, 700, 200, 100).
cost_breakdown(puerto_princesa, 1600, 900, 1200, 400, 100).
cost_breakdown(tagaytay, 1400, 800, 700, 300, 100).
cost_breakdown(lake_sebu, 1000, 600, 500, 300, 100).
cost_breakdown(camiguin, 1500, 800, 1000, 400, 100).

% FAMILY FRIENDLINESS SCORE (1-5)
family_score(boracay, 4).
family_score(palawan, 5).
family_score(cebu, 5).
family_score(bohol, 5).
family_score(baguio, 5).
family_score(siargao, 3).
family_score(vigan, 5).
family_score(batanes, 4).
family_score(davao, 5).
family_score(manila, 4).
family_score(coron, 4).
family_score(sagada, 3).
family_score(banaue, 4).
family_score(zambales, 4).
family_score(iloilo, 5).
family_score(mount_apo, 2).       % Too extreme for most families
family_score(tubbataha_reef, 3).  % Requires advanced diving skills
family_score(mayon_volcano, 3).   % Challenging climb
family_score(apo_reef, 3).        % Remote location
family_score(elnido, 4).          % Good for families with older kids
family_score(apo_island, 5).      % Great for family snorkeling
family_score(mount_pulag, 3).     % High altitude, challenging
family_score(taal_volcano, 4).    % Family-friendly with horseback option
family_score(puerto_princesa, 5). % Excellent for families
family_score(tagaytay, 5).        % Perfect for families
family_score(lake_sebu, 4).       % Good for adventurous families
family_score(camiguin, 5).        % Great for families

% ADVENTURE LEVEL (1-5)
adventure_level(boracay, 2).
adventure_level(palawan, 4).
adventure_level(cebu, 3).
adventure_level(bohol, 3).
adventure_level(baguio, 2).
adventure_level(siargao, 4).
adventure_level(vigan, 1).
adventure_level(batanes, 3).
adventure_level(davao, 4).
adventure_level(manila, 1).
adventure_level(coron, 5).
adventure_level(sagada, 4).
adventure_level(banaue, 3).
adventure_level(zambales, 3).
adventure_level(iloilo, 2).
adventure_level(mount_apo, 5).       % Extreme mountain climbing
adventure_level(tubbataha_reef, 5).  % Extreme diving
adventure_level(mayon_volcano, 5).   % Extreme volcano climbing
adventure_level(apo_reef, 4).        % Advanced diving
adventure_level(elnido, 4).          % Island adventure
adventure_level(apo_island, 3).      % Moderate marine adventure
adventure_level(mount_pulag, 4).     % High altitude adventure
adventure_level(taal_volcano, 3).    % Moderate volcano adventure
adventure_level(puerto_princesa, 3). % Moderate nature adventure
adventure_level(tagaytay, 2).        % Relaxing scenic adventure
adventure_level(lake_sebu, 4).       % Waterfalls and zipline adventure
adventure_level(camiguin, 3).        % Island nature adventure


% BEST TIME TO VISIT (months)
best_time(boracay, [11, 12, 1, 2, 3, 4]).
best_time(palawan, [11, 12, 1, 2, 3, 4]).
best_time(cebu, [1, 2, 3, 4, 5, 12]).
best_time(bohol, [1, 2, 3, 4, 5, 12]).
best_time(baguio, [1, 2, 3, 4, 11, 12]).
best_time(siargao, [3, 4, 5, 6, 7, 8, 9, 10]).
best_time(vigan, [1, 2, 3, 4, 11, 12]).
best_time(batanes, [3, 4, 5, 6, 7, 8, 9]).
best_time(davao, [1, 2, 3, 4, 12]).
best_time(manila, [1, 2, 3, 4, 12]).
best_time(coron, [11, 12, 1, 2, 3, 4]).
best_time(sagada, [1, 2, 3, 4, 11, 12]).
best_time(banaue, [4, 5, 6, 7, 8]).
best_time(zambales, [1, 2, 3, 4, 11, 12]).
best_time(iloilo, [1, 2, 3, 4, 12]).
best_time(mount_apo, [1, 2, 3, 4, 5]).        % Dry season for climbing
best_time(tubbataha_reef, [3, 4, 5, 6]).      % Liveaboard season
best_time(mayon_volcano, [1, 2, 3, 4, 5]).    % Dry season
best_time(apo_reef, [3, 4, 5, 6, 7, 8, 9]).   % Calmer seas
best_time(elnido, [1, 2, 3, 4, 5, 6, 11, 12]). % Extended good season
best_time(apo_island, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]). % Year-round
best_time(mount_pulag, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]). % Year-round
best_time(taal_volcano, [1, 2, 3, 4, 5, 6, 11, 12]). % Good weather months
best_time(puerto_princesa, [1, 2, 3, 4, 5, 6, 11, 12]). % Extended season
best_time(tagaytay, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]). % Year-round cool
best_time(lake_sebu, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]). % Year-round
best_time(camiguin, [1, 2, 3, 4, 5, 6, 11, 12]). % Good weather months

% ACTIVITY CATEGORIES
activity_category(beach_activities, beach).
activity_category(island_hopping, beach).
activity_category(surfing, beach).
activity_category(water_sports, beach).
activity_category(diving, nature).
activity_category(trekking, nature).
activity_category(wildlife_viewing, nature).
activity_category(hiking, mountain).
activity_category(caving, mountain).
activity_category(cultural_tours, historical).
activity_category(historical_sites, historical).
activity_category(shopping, city).
activity_category(food_tours, city).
activity_category(nightlife, city).

type_alias(mountain, nature).
type_alias(mountain, mountain).
type_alias(nature, nature).
type_alias(nature, mountain).



% Main conversation flow
conversational_recommender :-
    clear_screen,
    display_welcome,
    ask_preferences.

% Clear screen (GNU Prolog compatible)
clear_screen :-
    write('\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n').

% Display welcome message
display_welcome :-
    write('=============================================================='), nl,
    write('           TOURIST DESTINATION RECOMMENDER                    '), nl,
    write('                   (Philippines Edition)                      '), nl,
    write('=============================================================='), nl, nl,
    write('Hello! I will help you find the perfect tourist destination.'), nl,
    write('I will ask you a few questions to understand your preferences.'), nl, nl.

% Ask user preferences in conversation style
ask_preferences :-
    ask_type_preference,
    ask_family_trip,
    ask_adventure_level,
    ask_budget,
    ask_travel_month,
    generate_recommendations.

% Ask about destination type preference
ask_type_preference :-
    write('Lets start with what type of destination you prefer:'), nl,
    write('1. Beach destinations (sun, sand, sea)'), nl,
    write('2. Nature destinations (mountains, forests, wildlife)'), nl,
    write('3. City destinations (urban, shopping, nightlife)'), nl,
    write('4. Historical destinations (heritage, culture, architecture)'), nl,
    write('5. Mountain destinations (cool weather, hiking, scenery)'), nl,
    write('6. Any type (Im open to all)'), nl,
    write('Enter your choice (1-6): '),
    read(TypeChoice),
    process_type_choice(TypeChoice, Type),
    asserta(user_preference(type, Type)),
    nl.

process_type_choice(1, beach) :- !.
process_type_choice(2, nature) :- !.
process_type_choice(3, city) :- !.
process_type_choice(4, historical) :- !.
process_type_choice(5, mountain) :- !.
process_type_choice(6, any) :- !.
process_type_choice(_, any) :-
    write('Invalid choice. Assuming any type.'), nl.

% Ask if this is a family trip
ask_family_trip :-
    write('Is this a family trip? (yes/no): '),
    read(Answer),
    (Answer == yes -> 
        asserta(user_preference(family_trip, yes)),
        write('Great! I will prioritize family-friendly destinations.'), nl
        ;
        asserta(user_preference(family_trip, no)),
        write('Okay, I wont filter by family-friendliness.'), nl
    ),
    nl.

% Ask about adventure level preference
ask_adventure_level :-
    write('What level of adventure are you looking for?'), nl,
    write('1. Relaxing (chill, easy activities)'), nl,
    write('2. Moderate (some adventure, comfortable pace)'), nl,
    write('3. Adventurous (active, challenging activities)'), nl,
    write('4. Extreme (thrill-seeking, physically demanding)'), nl,
    write('5. Any level (Im flexible)'), nl,
    write('Enter your choice (1-5): '),
    read(AdventureChoice),
    process_adventure_choice(AdventureChoice, Level),
    asserta(user_preference(adventure_level, Level)),
    nl.

process_adventure_choice(1, relaxing) :- !.
process_adventure_choice(2, moderate) :- !.
process_adventure_choice(3, adventurous) :- !.
process_adventure_choice(4, extreme) :- !.
process_adventure_choice(5, any) :- !.
process_adventure_choice(_, any) :-
    write('Invalid choice. Assuming any adventure level.'), nl.

% Ask about budget
ask_budget :-
    write('What is your total budget for the trip (in PHP)? '),
    read(Budget),
    (Budget < 5000 ->
        write('Your budget is quite low. Let me show you the most affordable options.'), nl,
        asserta(user_preference(budget, Budget)),
        asserta(user_preference(budget_low, yes))
        ;
        asserta(user_preference(budget, Budget)),
        asserta(user_preference(budget_low, no)),
        format('Great! With PHP ~w, we can find some wonderful destinations.', [Budget]), nl
    ),
    nl.

% Ask about travel month
ask_travel_month :-
    write('Which month are you planning to travel? (Enter 1-12 for Jan-Dec): '),
    read(Month),
    (between(1, 12, Month) ->
        asserta(user_preference(month, Month)),
        format('Traveling in month ~w. I will consider the best time to visit.', [Month]), nl
        ;
        asserta(user_preference(month, any)),
        write('Invalid month. I wont filter by travel time.'), nl
    ),
    nl.




matches_preferences(Dest) :-
    destination(Dest, Type, _, _, _),
    filter_by_type(Type),
    filter_by_family(Dest),
    filter_by_adventure(Dest),
    filter_by_month(Dest).


explain_budget_insufficient(PreferenceMatches, Budget) :-
    write('Sorry, your budget is insufficient for the destinations that match your preferences.'), nl, nl,

    findall(Cost-Dest, (
        member(Dest, PreferenceMatches),
        destination(Dest, _, _, _, Cost)
    ), Costs),

    keysort(Costs, Sorted),
    Sorted = [CheapestCost-CheapestDest | _],

    format('The cheapest destination that fits your preferences is ~w.', [CheapestDest]), nl,
    format('Daily cost: PHP ~w', [CheapestCost]), nl,
    format('Your budget: PHP ~w', [Budget]), nl, nl,

    NeededDays is ceiling(CheapestCost / Budget),
    NeededBudget is CheapestCost,

    format('You need at least PHP ~w to stay for 1 day.', [NeededBudget]), nl, nl,

    write('Here are some popular alternatives you may consider:'), nl, nl,
    show_top_popular_destinations.



explain_no_preference_match :-
    write('Sorry, no destinations match all your selected preferences.'), nl,
    write('Your preferences may be too specific.'), nl, nl,
    write('Here are some popular destinations instead:'), nl, nl,
    show_top_popular_destinations.


show_top_popular_destinations :-
    findall(Score-Dest, (
        destination(Dest, _, _, _, Cost),
        family_score(Dest, Fam),
        adventure_level(Dest, Adv),
        Score is Fam + Adv - (Cost / 2000)
    ), Scored),

    keysort(Scored, Sorted),
    reverse(Sorted, Top),
    take(3, Top, Top3),

    show_alternative_destinations(Top3).





% ==================== RECOMMENDATION ENGINE ====================

% Generate recommendations based on preferences
generate_recommendations :-
    write('\n=============================================================='), nl,
    write('              ANALYZING YOUR PREFERENCES...                  '), nl,
    write('=============================================================='), nl, nl,

    user_preference(budget, Budget),
    

    findall(Dest, matches_preferences(Dest), PreferenceMatches),
    

    findall(Dest, (
        matches_preferences(Dest),
        destination(Dest, _, _, _, DailyCost),
        MaxDays is floor(Budget / DailyCost),
        MaxDays >= 1
    ), BudgetMatches),

     ( BudgetMatches \= [] ->
        show_recommendations(BudgetMatches)
    ;
      PreferenceMatches \= [] ->
        explain_budget_insufficient(PreferenceMatches, Budget)
    ;
        explain_no_preference_match
    ),
    
    retractall(user_preference(_, _)),
    ask_another_recommendation.

% Filter by type preference
filter_by_type(Type) :-
    user_preference(type, PrefType),
    (PrefType == any -> true ; Type == PrefType).

% Filter by family preference
filter_by_family(Dest) :-
    user_preference(family_trip, FamilyPref),
    (FamilyPref == no -> true ;
     FamilyPref == yes ->
        family_score(Dest, Score),
        Score >= 4
    ).


filter_by_adventure(Dest) :-
    user_preference(adventure_level, AdventurePref),
    (AdventurePref == any -> true ;
     AdventurePref == relaxing ->
        adventure_level(Dest, Score),
        Score =< 2
     ;
     AdventurePref == moderate ->
        adventure_level(Dest, Score),
        Score >= 2, Score =< 3
     ;
     AdventurePref == adventurous ->
        adventure_level(Dest, Score),
        Score >= 3, Score =< 4
     ;
     AdventurePref == extreme ->
        adventure_level(Dest, Score),
        Score >= 4
    ).


filter_by_month(Dest) :-
    user_preference(month, MonthPref),
    (MonthPref == any -> true ;
     best_time(Dest, Months),
     member(MonthPref, Months)
    ).

% Calculate maximum days based on budget
calculate_max_days(_Dest, DailyCost, MaxDays) :-
    user_preference(budget, Budget),
    MaxDaysFloat is Budget / DailyCost,
    MaxDays is floor(MaxDaysFloat).

% Show recommendations
show_recommendations(Destinations) :-
    user_preference(budget_low, BudgetLow),
    (BudgetLow == yes ->
        write('BUDGET ALERT: Your budget is limited. Here are the most affordable options:'), nl, nl,
        show_budget_options(Destinations)
        ;
        write('Based on your preferences, here are my recommendations:'), nl, nl,
        show_detailed_recommendations(Destinations)
    ).


show_detailed_recommendations([]).
show_detailed_recommendations([Dest|Rest]) :-
    destination(Dest, Type, Tags, Desc, DailyCost),
    cost_breakdown(Dest, Accom, Food, Activities, Transport, Misc),
    family_score(Dest, FamilyScore),
    adventure_level(Dest, AdventureScore),
    user_preference(budget, Budget),
    
    calculate_max_days(Dest, DailyCost, MaxDays),
    TotalCost is MaxDays * DailyCost,
    
    % Display destination card
    write('=============================================================='), nl,
    format('DESTINATION: ~w', [Dest]), nl,
    format('Type: ~w | Family Score: ~w/5 | Adventure: ~w/5', [Type, FamilyScore, AdventureScore]), nl,
    format('Description: ~w', [Desc]), nl,
    format('Tags: ~w', [Tags]), nl,
    write('--------------------------------------------------------------'), nl,
    write('COST BREAKDOWN (per day):'), nl,
    format('  Accommodation: PHP ~w', [Accom]), nl,
    format('  Food: PHP ~w', [Food]), nl,
    format('  Activities: PHP ~w', [Activities]), nl,
    format('  Transportation: PHP ~w', [Transport]), nl,
    format('  Miscellaneous: PHP ~w', [Misc]), nl,
    format('  TOTAL PER DAY: PHP ~w', [DailyCost]), nl,
    write('--------------------------------------------------------------'), nl,
    format('WITH YOUR BUDGET OF PHP ~w:', [Budget]), nl,
    format('  You can stay for ~w days', [MaxDays]), nl,
    format('  Estimated total cost: PHP ~w', [TotalCost]), nl,
    write('=============================================================='), nl, nl,
    
    show_detailed_recommendations(Rest).


show_budget_options(Destinations) :-
    % Sort destinations by daily cost
    predsort(compare_by_cost, Destinations, SortedDestinations),
    show_budget_list(SortedDestinations).

compare_by_cost(>, D1, D2) :-
    destination(D1, _, _, _, Cost1),
    destination(D2, _, _, _, Cost2),
    Cost1 > Cost2.
compare_by_cost(<, D1, D2) :-
    destination(D1, _, _, _, Cost1),
    destination(D2, _, _, _, Cost2),
    Cost1 < Cost2.
compare_by_cost(=, D1, D2) :-
    destination(D1, _, _, _, Cost),
    destination(D2, _, _, _, Cost).

show_budget_list([]).
show_budget_list([Dest|Rest]) :-
    destination(Dest, Type, _Tags, Desc, DailyCost),
    cost_breakdown(Dest, Accom, Food, Activities, Transport, Misc),
    user_preference(budget, _Budget),
    
    calculate_max_days(Dest, DailyCost, MaxDays),
    
    format('~w (~w)', [Dest, Type]), nl,
    format('  ~w', [Desc]), nl,
    format('  Daily cost: PHP ~w', [DailyCost]), nl,
    format('  You can stay: ~w days', [MaxDays]), nl,
    format('  Breakdown: Accom(PHP~w) + Food(PHP~w) + Activities(PHP~w) + Transport(PHP~w) + Misc(PHP~w)', 
           [Accom, Food, Activities, Transport, Misc]), nl, nl,
    
    show_budget_list(Rest).

% Show message when no recommendations found
show_no_recommendations :-
    write('Sorry, I could not find destinations matching all your preferences.'), nl,
    write('Let me suggest some alternatives:'), nl, nl,
    
    % Get all destinations sorted by overall score
    findall(Score-Dest, (
        destination(Dest, _, _, _, DailyCost),
        family_score(Dest, FamilyScore),
        adventure_level(Dest, AdventureScore),
        Score is (5 - (DailyCost / 2000)) + FamilyScore + AdventureScore
    ), ScoredDestinations),
    
    keysort(ScoredDestinations, Sorted),
    reverse(Sorted, TopDestinations),
    take(3, TopDestinations, Top3),
    
    write('TOP 3 POPULAR DESTINATIONS:'), nl, nl,
    show_alternative_destinations(Top3).

take(0, _, []) :- !.
take(_, [], []) :- !.
take(N, [H|T], [H|R]) :-
    N1 is N - 1,
    take(N1, T, R).

show_alternative_destinations([]).
show_alternative_destinations([_-Dest|Rest]) :-
    destination(Dest, Type, _, Desc, DailyCost),
    family_score(Dest, FamilyScore),
    adventure_level(Dest, AdventureScore),
    
    format('~w (~w)', [Dest, Type]), nl,
    format('  ~w', [Desc]), nl,
    format('  Daily cost: PHP ~w | Family: ~w/5 | Adventure: ~w/5', 
           [DailyCost, FamilyScore, AdventureScore]), nl, nl,
    
    show_alternative_destinations(Rest).

% Ask if user wants another recommendation
ask_another_recommendation :-
    nl,
    write('=============================================================='), nl,
    write('Would you like another recommendation? (yes/no): '),
    read(Answer),
    (Answer == yes ->
        nl, nl,
        conversational_recommender
        ;
        write('\nThank you for using the Tourist Destination Recommender!'), nl,
        write('Safe travels!'), nl
    ).

% ==================== DIRECT QUERY INTERFACE ====================

% Direct query interface for testing
direct_recommendation(Type, Family, Adventure, Budget, Month) :-
    asserta(user_preference(type, Type)),
    asserta(user_preference(family_trip, Family)),
    asserta(user_preference(adventure_level, Adventure)),
    asserta(user_preference(budget, Budget)),
    (Family == yes -> asserta(user_preference(budget_low, no)) ; true),
    asserta(user_preference(month, Month)),
    generate_recommendations,
    retractall(user_preference(_, _)).

% Quick recommendation based on budget only
quick_budget_recommendation(Budget) :-
    write('Quick Recommendations for Budget: PHP '), write(Budget), nl, nl,
    
    findall(Days-Dest, (
        destination(Dest, _Type, _, _, DailyCost),
        Days is floor(Budget / DailyCost),
        Days >= 1
    ), Destinations),
    
    keysort(Destinations, Sorted),
    reverse(Sorted, BestDestinations),
    
    show_quick_recommendations(BestDestinations).

show_quick_recommendations([]).
show_quick_recommendations([Days-Dest|Rest]) :-
    destination(Dest, Type, _, Desc, DailyCost),
    family_score(Dest, FamilyScore),
    
    format('~w (~w):', [Dest, Type]), nl,
    format('  ~w', [Desc]), nl,
    format('  Daily cost: PHP ~w', [DailyCost]), nl,
    format('  With your budget: ~w days', [Days]), nl,
    format('  Family score: ~w/5', [FamilyScore]), nl, nl,
    
    show_quick_recommendations(Rest).



% Display main menu
display_main_menu :-
    clear_screen,
    write('=============================================================='), nl,
    write('           TOURIST DESTINATION RECOMMENDER                   '), nl,
    write('                      MAIN MENU                              '), nl,
    write('=============================================================='), nl, nl,
    write('1. Start Conversational Recommender'), nl,
    write('2. Quick Budget Recommendation'), nl,
    write('3. List All Destinations'), nl,
    write('4. Find Cheapest Destination'), nl,
    write('5. Find Most Family-Friendly'), nl,
    write('6. Exit'), nl,
    nl,
    write('Enter your choice (1-6): ').

% Process menu choice
process_menu_choice(1) :-
    conversational_recommender.
    
process_menu_choice(2) :-
    nl,
    write('Enter your budget in PHP: '),
    read(Budget),
    quick_budget_recommendation(Budget),
    press_any_key,
    main_menu.

process_menu_choice(3) :-
    nl,
    write('=============================================================='), nl,
    write('                   ALL DESTINATIONS                         '), nl,
    write('=============================================================='), nl, nl,
    list_all_destinations,
    press_any_key,
    main_menu.

process_menu_choice(4) :-
    nl,
    write('=============================================================='), nl,
    write('              MOST AFFORDABLE DESTINATIONS                  '), nl,
    write('=============================================================='), nl, nl,
    find_cheapest_destinations,
    press_any_key,
    main_menu.

process_menu_choice(5) :-
    nl,
    write('=============================================================='), nl,
    write('                MOST FAMILY-FRIENDLY                        '), nl,
    write('=============================================================='), nl, nl,
    find_most_family_friendly,
    press_any_key,
    main_menu.

process_menu_choice(6) :-
    write('Thank you for using the Tourist Destination Recommender!'), nl,
    write('Safe travels!'), nl,
    halt.

process_menu_choice(_) :-
    write('Invalid choice. Please try again.'), nl,
    press_any_key,
    main_menu.


list_all_destinations :-
    findall(Dest, destination(Dest, _, _, _, _), Dests),
    list_destinations(Dests).

list_destinations([]).
list_destinations([Dest|Rest]) :-
    destination(Dest, Type, Tags, Desc, DailyCost),
    family_score(Dest, FamilyScore),
    adventure_level(Dest, AdventureScore),
    
    format('~w', [Dest]), nl,
    format('  Type: ~w', [Type]), nl,
    format('  Description: ~w', [Desc]), nl,
    format('  Daily Cost: PHP ~w', [DailyCost]), nl,
    format('  Family: ~w/5 | Adventure: ~w/5', [FamilyScore, AdventureScore]), nl,
    format('  Tags: ~w', [Tags]), nl, nl,
    
    list_destinations(Rest).

find_cheapest_destinations :-
    findall(Cost-Dest, (
        destination(Dest, _, _, _, DailyCost),
        Cost = DailyCost
    ), Destinations),
    
    keysort(Destinations, Sorted),
    take(5, Sorted, Cheapest),
    
    write('TOP 5 MOST AFFORDABLE DESTINATIONS:'), nl, nl,
    show_cheapest(Cheapest).

show_cheapest([]).
show_cheapest([Cost-Dest|Rest]) :-
    destination(Dest, Type, _, Desc, _),
    family_score(Dest, FamilyScore),
    
    format('~w (~w) - PHP ~w per day', [Dest, Type, Cost]), nl,
    format('  ~w', [Desc]), nl,
    format('  Family score: ~w/5', [FamilyScore]), nl, nl,
    
    show_cheapest(Rest).


find_most_family_friendly :-
    findall(Score-Dest, (
        destination(Dest, _, _, _, _),
        family_score(Dest, Score)
    ), Destinations),
    
    keysort(Destinations, Sorted),
    reverse(Sorted, FamilyFriendly),
    take(5, FamilyFriendly, Top5),
    
    write('TOP 5 MOST FAMILY-FRIENDLY DESTINATIONS:'), nl, nl,
    show_family_friendly(Top5).

show_family_friendly([]).
show_family_friendly([Score-Dest|Rest]) :-
    destination(Dest, Type, _, Desc, DailyCost),
    
    format('~w (~w) - ~w/5 family score', [Dest, Type, Score]), nl,
    format('  ~w', [Desc]), nl,
    format('  Daily cost: PHP ~w', [DailyCost]), nl, nl,
    
    show_family_friendly(Rest).


press_any_key :-
    nl,
    write('Press Enter to continue...'),
    read(_).


main_menu :-
    display_main_menu,
    read(Choice),
    process_menu_choice(Choice).



start :-
    main_menu.

% Welcome message when loading
:- initialization((
    write('=============================================================='), nl,
    write('           TOURIST DESTINATION RECOMMENDER                   '), nl,
    write('                 Enhanced Version Loaded!                    '), nl,
    write('=============================================================='), nl,
    write('Type "start." to begin.'), nl,
    write('Type "conversational_recommender." for direct chat.'), nl,
    write('=============================================================='), nl
)).