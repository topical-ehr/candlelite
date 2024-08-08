import { test } from "vitest";
import { fhir } from "./utils";

test("Basic CRUD", async ({ expect }) => {
    expect(1).toBe(1);

    // Test create
    const createResp = await fhir("POST", "/Patient", {
        resourceType: "Patient",
        name: [{ family: "Doe", given: ["John"] }],
    });
    const id = createResp.id;
    expect(createResp.resourceType).toBe("Patient");
    expect(createResp.name[0].family).toBe("Doe");
    expect(createResp.name[0].given[0]).toBe("John");

    // Test update
    const updateResp = await fhir("PUT", `/Patient/${id}`, {
        resourceType: "Patient",
        id,
        name: [{ family: "Doe", given: ["Jane"] }],
    });
    expect(updateResp.resourceType).toBe("Patient");
    expect(updateResp.name[0].family).toBe("Doe");
    expect(updateResp.name[0].given[0]).toBe("Jane");

    // Test read
    const readResp = await fhir("GET", `/Patient/${id}`);
    expect(readResp.resourceType).toBe("Patient");
    expect(readResp.name[0].family).toBe("Doe");
    expect(readResp.name[0].given[0]).toBe("Jane");

    // Test history
    const historyResp = await fhir("GET", `/Patient/${id}/_history`);
    expect(historyResp.resourceType).toBe("Bundle");
    expect(historyResp.entry.length).toBe(2);
    expect(historyResp.entry[0].resource.name[0].given[0]).toBe("John");
    expect(historyResp.entry[1].resource.name[0].given[0]).toBe("Jane");

    // Test delete
    const deleteResp = await fhir("DELETE", `/Patient/${id}`);

    // Test read after delete
    const readAfterDeleteResp = await fhir("GET", `/Patient/${id}`, null, {
        expectedStatusCode: 404,
    });
    expect(readAfterDeleteResp.resourceType).toBe("OperationOutcome");
    expect(readAfterDeleteResp.issue[0].code).toBe("not-found");

    // Test history after delete
    const historyAfterDeleteResp = await fhir("GET", `/Patient/${id}/_history`);
    console.log({
        historyAfterDeleteResp: JSON.stringify(historyAfterDeleteResp),
    });
    expect(historyAfterDeleteResp.resourceType).toBe("Bundle");
    expect(historyAfterDeleteResp.entry.length).toBe(3);
    expect(historyAfterDeleteResp.entry[0].resource.name[0].given[0]).toBe(
        "John"
    );
    expect(historyAfterDeleteResp.entry[1].resource.name[0].given[0]).toBe(
        "Jane"
    );
    expect(historyAfterDeleteResp.entry[2].request.method).toBe("DELETE");
    expect(historyAfterDeleteResp.entry[2].resource).toBeUndefined();
});
