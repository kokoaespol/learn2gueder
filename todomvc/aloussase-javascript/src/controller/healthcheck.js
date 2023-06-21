import { Router } from "express"

export const healthcheck = () => {
    const healthcheckRouter = new Router();

    healthcheckRouter.get("/", (_, res) => {
        return res.status(204).end();
    });

    return healthcheckRouter;
}