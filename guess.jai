#import "Basic";
#import "Random";

main :: () {
    high :: 100;
    random_seed(current_time_monotonic().low);
    a_variable := random_get_zero_to_one_open();
    print("A is of type %, value %.\n", type_of(a_variable), a_variable);
}
