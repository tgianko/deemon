from type_enum import *

TypeEnum.reset()

# Always the same value for one user, so actually user constant
SEM_TYPE_USER_UNIQUE = TypeEnum("user_unique")
SEM_TYPE_SESSION_UNIQUE = TypeEnum("session_unique") # Value once per user
SEM_TYPE_CONSTANT = TypeEnum("constant") # All values are equal, for all users
SEM_TYPE_UNCERTAIN = TypeEnum("uncertain")  # Anything else

# Gets a list containing an arbitrary amount of tupels, each holding "user" and "value"
def infer_semantic_type(tuples):
    if len(tuples) == 0:
        return SEM_TYPE_UNCERTAIN

    first_val = tuples[0]["value"]
    values_per_user = {}
    permitted_duplicates = 0 # for user unique a value might occur multiple times with the same user key
    all_values = set()
    constant = True
    for item in tuples:
        val = item["value"]
        user_key = item["user"]

        # Check whether the value is constant
        if val != first_val:
            constant = False

        # User unique
        if user_key not in values_per_user:
            values_per_user[user_key] = set()

        if val not in all_values:
            values_per_user[user_key].add(val)
        elif val in values_per_user[user_key]:
            permitted_duplicates += 1

        # Session unique
        all_values.add(val)

    # When constant, it cannot be unique in any way
    if constant:
        return SEM_TYPE_CONSTANT

    # Everything that is session unique is also user unique, therefore this
    # has to be checked first
    if len(all_values) == len(tuples):
        return SEM_TYPE_SESSION_UNIQUE

    # If it is user unique (occurs at least two time for one user)
    if len(all_values) + permitted_duplicates == len(tuples):
        return SEM_TYPE_USER_UNIQUE

    # Example for uncertain case: (user1, a), (user1, b), (user2, b)
    return SEM_TYPE_UNCERTAIN
