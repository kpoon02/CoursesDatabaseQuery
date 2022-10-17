export interface Query {
	WHERE: Where;
	OPTIONS: Options;
}

export interface Options {
	COLUMNS: string[];
	ORDER?: string;
}

export interface Or {
	AND?: And[];
	OR?: Or[];
	LT?: Lt;
	GT?: Gt;
	EQ?: Eq;
	IS?: Is;
	NOT?: Not;
}

export interface Where {
	AND?: And[];
	OR?: Or[];
	LT?: Lt;
	GT?: Gt;
	EQ?: Eq;
	IS?: Is;
	NOT?: Not;
}

export interface And {
	AND?: And[];
	OR?: Or[];
	LT?: Lt;
	GT?: Gt;
	EQ?: Eq;
	IS?: Is;
	NOT?: Not;
}

export interface Lt {
	[key: string]: number;
}

export interface Gt {
	[key: string]: number;
}

export interface Eq {
	[key: string]: number;
}

export interface Not {
	AND?: And[];
	OR?: Or[];
	LT?: Lt;
	GT?: Gt;
	EQ?: Eq;
	IS?: Is;
	NOT?: Not;
}

export interface Is {
	[key: string]: string;
}
