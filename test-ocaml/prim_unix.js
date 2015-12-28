//Provides: unix_gethostname
function unix_gethostname() {
    return {t: 0, c:"hostname", l:8}
}

//Provides: unix_environment
function unix_environment() {
    return [0];
}
