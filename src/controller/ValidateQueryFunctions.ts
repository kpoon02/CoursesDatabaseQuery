import {Query, Where} from "../model/Query";

export class ValidateQueryFunctions {
	// Validate WHERE
	public static validateWHERE(q: Query): boolean {
		if (q.WHERE === null) {
			return true;
		} else {
			return this.validateFilter(q, q.WHERE);
		}
	}

	// Validate Filter
	public static validateFilter(q: Query, filter: Where): boolean {
		// check if COLUMNS is empty, if empty, cannot retrieve a datasetID
		if (filter.AND !== undefined && filter.AND.length !== 0) {
			return (this.validateFilter(q, filter.AND[0]) && this.validateFilter(q, filter.AND[1]));
		} else if (filter.OR !== undefined && filter.OR.length !== 0) {
			return (this.validateFilter(q, filter.OR[0]) && this.validateFilter(q, filter.OR[1]));
		} else if (filter.LT !== undefined && filter.LT.length !== 0) {
			return this.validateMComparison(q, filter.LT);
		} else if (filter.GT !== undefined && filter.GT.length !== 0) {
			return this.validateMComparison(q, filter.GT);
		} else if (filter.EQ !== undefined && filter.EQ.length !== 0) {
			return this.validateMComparison(q, filter.EQ);
		} else if (filter.IS !== undefined && filter.IS !== null) {
			return this.validateSComparison(q, filter.IS);
		} else if (filter.NOT !== undefined && filter.NOT !== null) {
			return this.validateFilter(q, filter.NOT);
		}
		return false;
	}

	// Validate SComparison
	public static validateSComparison(q: Query, sComp: object): boolean {
		let regex: RegExp = /^\*?[^*]*\*?$/;
		if (sComp === undefined) {
			return false;
		}
		if (Object.keys(sComp).length > 1) {
			// check that the keys are the same if there are multiple, and keys are skeys
			if (!Object.keys(sComp).every((val, i, arr) => val === arr[0])) {
				return false;
			} else if (!this.sKey(Object.keys(sComp)[0], q.OPTIONS.COLUMNS[0].split("_")[0])) {
				return false;
			} else if (typeof Object.values(sComp)[Object.values(sComp).length - 1] !== "string") {
				return false;
			} else if (!regex.test(Object.values(sComp)[Object.values(sComp).length - 1])) {
				return false;
			}
		} else if (Object.keys(sComp).length === 0) {
			return false;
		} else {
			// only one key
			if (!this.sKey(Object.keys(sComp)[0], q.OPTIONS.COLUMNS[0].split("_")[0])) {
				return false;
			}
			if (typeof Object.values(sComp)[0] !== "string") {
				return false;
			}
			if (!regex.test(Object.values(sComp)[0])) {
				return false;
			}
		}
		return true;
	}

	// Validate MComparison
	public static validateMComparison(q: Query, mComp: object): boolean {
		if (mComp === undefined) {
			return false;
		}
		if (Object.keys(mComp).length > 1) {
			// check that the keys are the same if there are multiple, and keys are mkeys
			if (!Object.keys(mComp).every((val, i, arr) => val === arr[0])) {
				return false;
			} else if (!this.mKey(Object.keys(mComp)[0], q.OPTIONS.COLUMNS[0].split("_")[0])) {
				return false;
			} else if (typeof Object.values(mComp)[Object.values(mComp).length - 1] !== "number") {
				return false;
			}
		} else if (Object.keys(mComp).length === 0) {
			return false;
		} else {
			// only one key
			if (!this.mKey(Object.keys(mComp)[0], q.OPTIONS.COLUMNS[0].split("_")[0])) {
				return false;
			}
			if (typeof Object.values(mComp)[0] !== "number") {
				return false;
			}
		}
		return true;
	}

	// Validate OPTIONS
	public static validateOPTIONS(q: Query): boolean {
		// check columns
		if (q.OPTIONS.COLUMNS === undefined) {
			return false;
		}
		if (q.OPTIONS.COLUMNS.length === 0) {
			return false;
		}
		if (Array.isArray(q.OPTIONS.COLUMNS)) {  // check if columns is an array
			for (let item of q.OPTIONS.COLUMNS) {
				// false if elements are not a string
				if (typeof item !== "string") {
					return false;
				}
				// false if elements are not keys
				if (!this.mKey(item, q.OPTIONS.COLUMNS[0].split("_")[0]) &&
					!this.sKey(item, q.OPTIONS.COLUMNS[0].split("_")[0])) {
					return false;
				}
			}
		}
		if (!Array.isArray(q.OPTIONS.COLUMNS)) {
			return false;
		}
		if (q.OPTIONS.COLUMNS.length === 0) {
			return false;
		}
		// check order
		if (q.OPTIONS.ORDER === undefined) {
			return true;
		}
		if (typeof q.OPTIONS.ORDER !== "string") {
			return false;
		}
		if (!this.mKey(q.OPTIONS.ORDER, q.OPTIONS.COLUMNS[0].split("_")[0]) &&
			!this.sKey(q.OPTIONS.ORDER, q.OPTIONS.COLUMNS[0].split("_")[0])) {
			return false;
		}
		if (!q.OPTIONS.COLUMNS.includes(q.OPTIONS.ORDER)) {
			return false;
		}
		return true;
	}

	// check if string is a mKey
	public static mKey(key: string, idString: string): boolean {
		if (key === idString.concat("_avg") || key === idString.concat("_pass") ||
			key === idString.concat("_fail") || key === idString.concat("_audit") ||
			key === idString.concat("_year")) {
			return true;
		}
		return false;
	}

	// check if string is a sKey
	public static sKey(key: string, idString: string): boolean {
		if (key === idString.concat("_dept") || key === idString.concat("_id") ||
			key === idString.concat("_instructor") || key === idString.concat("_title") ||
			key === idString.concat("_uuid")) {
			return true;
		}
		return false;
	}
}
