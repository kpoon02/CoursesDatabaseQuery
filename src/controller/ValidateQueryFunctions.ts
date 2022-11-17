import {Query, Where} from "../model/Query";
import {SectionDataset} from "../model/dataset/SectionDataset";
import {InsightError} from "./IInsightFacade";
import {RoomDataset} from "../model/dataset/RoomDataset";
import {ValidateQueryStrings} from "./ValidateQueryStrings";

export class ValidateQueryFunctions {
	public static getDatasetId(q: Query): string {
		if (!Array.isArray(q.OPTIONS.COLUMNS)) {
			throw new InsightError("Given dataset not found");
		}
		if (q.TRANSFORMATIONS === undefined) {
			return q.OPTIONS.COLUMNS[0].split("_")[0];
		}
		return q.TRANSFORMATIONS.GROUP[0].split("_")[0];
	}

	public static validateQuery(q: Query, datasets: SectionDataset[] | RoomDataset[], queryType: string): boolean {
		if (q.OPTIONS.COLUMNS.length === 0) {
			return false;
		} else {
			let datasetId: string;
			datasetId = this.getDatasetId(q);
			if (this.datasetIdInDisk(datasetId, datasets)) {
				return (
					this.validateWHERE(q, queryType) &&
					this.validateOPTIONS(q, queryType) &&
					this.validateTRANSFORMATIONS(q, queryType)
				);
			} else {
				return false;
			}
		}
	}

	public static datasetIdInDisk(datasetId: string, datasets: SectionDataset[] | RoomDataset[]): boolean {
		for (let dataset of datasets) {
			if (datasetId === dataset.getId()) {
				return true;
			}
		}
		return false;
	}

	public static validateWHERE(q: Query, queryType: string): boolean {
		if (Object.keys(q.WHERE).length === 0) {
			return true;
		} else if (Object.keys(q.WHERE).length > 1) {
			return false;
		} else {
			return this.validateFilter(q, q.WHERE, queryType);
		}
	}

	public static validateFilter(q: Query, filter: Where, queryType: string): boolean {
		if (filter.AND !== undefined && filter.AND.length !== 0) {
			for (let andFilter of filter.AND) {
				if (!this.validateFilter(q, andFilter, queryType)) {
					return false;
				}
			}
			return true;
		} else if (filter.OR !== undefined && filter.OR.length !== 0) {
			for (let orFilter of filter.OR) {
				if (!this.validateFilter(q, orFilter, queryType)) {
					return false;
				}
			}
			return true;
		} else if (filter.LT !== undefined && filter.LT.length !== 0) {
			return this.validateMComparison(q, filter.LT, queryType);
		} else if (filter.GT !== undefined && filter.GT.length !== 0) {
			return this.validateMComparison(q, filter.GT, queryType);
		} else if (filter.EQ !== undefined && filter.EQ.length !== 0) {
			return this.validateMComparison(q, filter.EQ, queryType);
		} else if (filter.IS !== undefined && filter.IS !== null) {
			return this.validateSComparison(q, filter.IS, queryType);
		} else if (filter.NOT !== undefined && filter.NOT !== null) {
			return this.validateFilter(q, filter.NOT, queryType);
		}
		return false;
	}

	public static validateSComparison(q: Query, sComp: object, queryType: string): boolean {
		let regex: RegExp = /^\*?[^*]*\*?$/;
		if (sComp === undefined) {
			return false;
		}
		if (Object.keys(sComp).length > 1) {
			if (!Object.keys(sComp).every((val, i, arr) => val === arr[0])) {
				return false;
			} else if (!ValidateQueryStrings.sKey(Object.keys(sComp)[0], this.getDatasetId(q), queryType)) {
				return false;
			} else if (typeof Object.values(sComp)[Object.values(sComp).length - 1] !== "string") {
				return false;
			} else if (!regex.test(Object.values(sComp)[Object.values(sComp).length - 1])) {
				return false;
			}
		} else if (Object.keys(sComp).length === 0) {
			return false;
		} else {
			if (typeof Object.values(sComp)[0] !== "string") {
				return false;
			}
			if (!ValidateQueryStrings.sKey(Object.keys(sComp)[0], this.getDatasetId(q), queryType)) {
				return false;
			}
			if (!regex.test(Object.values(sComp)[0])) {
				return false;
			}
		}
		return true;
	}

	public static validateMComparison(q: Query, mComp: object, queryType: string): boolean {
		if (mComp === undefined) {
			return false;
		}
		if (Object.keys(mComp).length > 1) {
			if (!Object.keys(mComp).every((val, i, arr) => val === arr[0])) {
				return false;
			} else if (!ValidateQueryStrings.mKey(Object.keys(mComp)[0], this.getDatasetId(q), queryType)) {
				return false;
			} else if (typeof Object.values(mComp)[Object.values(mComp).length - 1] !== "number") {
				return false;
			}
		} else if (Object.keys(mComp).length === 0) {
			return false;
		} else {
			if (typeof Object.values(mComp)[0] !== "number") {
				return false;
			}
			if (!ValidateQueryStrings.mKey(Object.keys(mComp)[0], this.getDatasetId(q), queryType)) {
				return false;
			}
		}
		return true;
	}

	public static validateOPTIONS(q: Query, queryType: string): boolean {
		if (q.OPTIONS.COLUMNS === undefined) {
			return false;
		}
		if (!Array.isArray(q.OPTIONS.COLUMNS)) {
			return false;
		}
		if (q.OPTIONS.COLUMNS.length === 0) {
			return false;
		}
		if (Array.isArray(q.OPTIONS.COLUMNS)) {
			for (let item of q.OPTIONS.COLUMNS) {
				if (
					!ValidateQueryStrings.mKey(item, this.getDatasetId(q), queryType) &&
					!ValidateQueryStrings.sKey(item, this.getDatasetId(q), queryType) &&
					!ValidateQueryStrings.validateIdStringOrApplyKey(item)
				) {
					return false;
				}
			}
		}
		let applyKeys = [];
		if (q.TRANSFORMATIONS?.GROUP !== undefined) {
			for (let item of q.OPTIONS.COLUMNS) {
				if (!q.TRANSFORMATIONS?.GROUP.includes(item)) {
					for (let applyRule of q.TRANSFORMATIONS.APPLY) {
						applyKeys.push(Object.keys(applyRule)[0]);
					}
					if (!applyKeys.includes(item)) {
						return false;
					}
				}
			}
		}
		return this.validateOrder(q, queryType);
	}

	private static validateOrder(q: Query, queryType: string): boolean {
		if (q.OPTIONS.ORDER === undefined) {
			return true;
		}
		if (typeof q.OPTIONS.ORDER === "string") {
			if (
				!ValidateQueryStrings.mKey(q.OPTIONS.ORDER, q.OPTIONS.COLUMNS[0].split("_")[0], queryType) &&
				!ValidateQueryStrings.sKey(q.OPTIONS.ORDER, q.OPTIONS.COLUMNS[0].split("_")[0], queryType) &&
				!ValidateQueryStrings.validateIdStringOrApplyKey(q.OPTIONS.ORDER)
			) {
				return false;
			}
			return q.OPTIONS.COLUMNS.includes(q.OPTIONS.ORDER);
		} else {
			if (q.OPTIONS.ORDER.keys === undefined || q.OPTIONS.ORDER.dir === undefined) {
				return false;
			}
			if (q.OPTIONS.ORDER.keys.length === 0) {
				return false;
			}
			for (let orderKeys of q.OPTIONS.ORDER.keys) {
				if (
					!ValidateQueryStrings.mKey(orderKeys, this.getDatasetId(q), queryType) &&
					!ValidateQueryStrings.sKey(orderKeys, this.getDatasetId(q), queryType) &&
					!ValidateQueryStrings.validateIdStringOrApplyKey(orderKeys)
				) {
					return false;
				}
			}
			for (let orderKeys of q.OPTIONS.ORDER.keys) {
				if (!q.OPTIONS.COLUMNS.includes(orderKeys)) {
					return false;
				}
			}
			if (q.OPTIONS.ORDER.dir !== "UP" && q.OPTIONS.ORDER.dir !== "DOWN") {
				return false;
			}
			return true;
		}
	}

	private static validateTRANSFORMATIONS(q: Query, queryType: string): boolean {
		if (q.TRANSFORMATIONS === undefined) {
			return true;
		} else {
			return this.validateGROUP(q, queryType) && this.validateAPPLY(q, queryType);
		}
	}

	private static validateGROUP(q: Query, queryType: string): boolean {
		if (q.TRANSFORMATIONS?.GROUP === undefined || q.TRANSFORMATIONS?.GROUP.length === 0) {
			return false;
		}
		for (let item of q.TRANSFORMATIONS.GROUP) {
			if (
				!ValidateQueryStrings.mKey(item, this.getDatasetId(q), queryType) &&
				!ValidateQueryStrings.sKey(item, this.getDatasetId(q), queryType)
			) {
				return false;
			}
		}
		return true;
	}

	private static validateAPPLY(q: Query, queryType: string): boolean {
		if (q.TRANSFORMATIONS?.APPLY === undefined) {
			return false;
		}
		if (q.TRANSFORMATIONS?.APPLY.length === 0) {
			return true;
		}
		if (q.TRANSFORMATIONS.APPLY !== undefined) {
			let applyKeys = new Set<string>();
			for (let applyRule of q.TRANSFORMATIONS.APPLY) {
				let applyKey = Object.keys(applyRule)[0];
				let applyToken = Object.keys(Object.values(applyRule)[0])[0];
				let key = Object.values(Object.values(applyRule)[0])[0];
				applyKeys.add(applyKey);
				if (
					!ValidateQueryStrings.validateIdStringOrApplyKey(applyKey) ||
					!ValidateQueryStrings.validateApplyToken(applyToken) ||
					(!ValidateQueryStrings.mKey(key, this.getDatasetId(q), queryType) &&
						!ValidateQueryStrings.sKey(key, this.getDatasetId(q), queryType))
				) {
					return false;
				}
				if (applyToken === "MAX" || applyToken === "MIN" || applyToken === "AVG" || applyToken === "SUM") {
					if (ValidateQueryStrings.sKey(key, this.getDatasetId(q), queryType)) {
						return false;
					}
				}
			}
			if (applyKeys.size === q.TRANSFORMATIONS.APPLY.length) {
				return true;
			} else {
				return false;
			}
		}
		throw new Error("Cannot validate APPLY");
	}
}
