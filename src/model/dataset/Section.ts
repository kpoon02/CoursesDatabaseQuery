import {JsonSection} from "../../controller/dataset/SectionDatasetController";
import {InsightError} from "../../controller/IInsightFacade";

export class Section {
	private readonly _dept: string; // JsonSection SUBJECT
	private readonly _id: string; // JsonSection COURSE
	private readonly _avg: number;
	private readonly _instructor: string; // JsonSection PROFESSOR
	private readonly _title: string;
	private readonly _pass: number;
	private readonly _fail: number;
	private readonly _audit: number;
	private readonly _uuid: string; // JsonSection ID
	private readonly _year: number;

	constructor(jsonSection: JsonSection) {
		this._dept = jsonSection.Subject;
		this._id = jsonSection.Course;
		this._avg = jsonSection.Avg;
		this._instructor = jsonSection.Professor;
		this._title = jsonSection.Title;
		this._pass = jsonSection.Pass;
		this._fail = jsonSection.Fail;
		this._audit = jsonSection.Audit;
		this._uuid = "" + jsonSection.id;
		if (jsonSection.Section === "overall") {
			this._year = 1900;
		} else {
			this._year = +jsonSection.Year;
		}
	}

	public get dept(): string {
		return this._dept;
	}

	public get id(): string {
		return this._id;
	}

	public get avg(): number {
		return this._avg;
	}

	public get instructor(): string {
		return this._instructor;
	}

	public get title(): string {
		return this._title;
	}

	public get pass(): number {
		return this._pass;
	}

	public get fail(): number {
		return this._fail;
	}

	public get audit(): number {
		return this._audit;
	}

	public get uuid(): string {
		return this._uuid;
	}

	public get year(): number {
		return this._year;
	}

	public getProperty(property: string): string | number {
		if (property === "dept") {
			return this._dept;
		} else if (property === "id") {
			return this._id;
		} else if (property === "avg") {
			return this._avg;
		} else if (property === "instructor") {
			return this._instructor;
		} else if (property === "title") {
			return this._title;
		} else if (property === "pass") {
			return this._pass;
		} else if (property === "fail") {
			return this._fail;
		} else if (property === "audit") {
			return this._audit;
		} else if (property === "uuid") {
			return this._uuid;
		} else if (property === "year") {
			return this._year;
		} else {
			throw new InsightError("Cannot retrieve field for this Section");
		}
	}
}
