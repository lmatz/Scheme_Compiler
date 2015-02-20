static char* allocate_protected_space(int size) {
	int page = getpagesize();
	int status;
	int aligned_size = ((size + page - 1) / page) * page;
	char* p = mmap(0, aligned_size + 2 * page, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, 0, 0);
	if (p == MAP_FAILED) { exit(1); }
	status = mprotect(p, page, PROT_NONE);
	if (status != 0) { exit(1); }
	status = mprotect(p + page + aligned_size, page, PROT_NONE);
	if (status != 0) { exit(1); }
	return p + page;
}
