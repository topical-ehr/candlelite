const baseUrl = process.env["FHIR_SERVER_BASE_URL"];

if (!baseUrl) {
    throw new Error("FHIR_SERVER_BASE_URL is not set");
}

console.info("FHIR server base url:", baseUrl);

export async function fhir(
    method: "GET" | "POST" | "PUT" | "DELETE",
    path: string,
    body?: unknown,
    opts?: {
        expectedStatusCode?: number;
        prefer?: "representation" | "minimal" | "OperationOutcome";
    }
) {
    const resp = await fetch(baseUrl + path, {
        method,
        body: body ? JSON.stringify(body) : undefined,
        headers: {
            Prefer: `return=${opts?.prefer ?? "representation"}`,
        },
    });
    console.log(`${method} ${baseUrl}${path} => ${resp.status}`);

    if (resp.status >= 500) {
        console.error(await resp.text());
        throw new Error(`Server error: ${resp.status}`);
    }

    if (opts?.expectedStatusCode && resp.status !== opts.expectedStatusCode) {
        throw new Error(
            `Unexpected status code: expected=${opts.expectedStatusCode}, received=${resp.status}`
        );
    }
    if (resp.status === 204) {
        return null;
    }

    const reply = await resp.json();
    console.log({ reply: JSON.stringify(reply) });
    return reply;
}
