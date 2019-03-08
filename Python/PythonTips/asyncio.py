import asyncio

@asyncio.coroutine
def mycor(ssleep =3):
    print('Corutine sleeps for {0} seconfs'.format(ssleep))
    yield from asyncio.sleep(ssleep)


loop = asyncio.get_event_loop()
loop.run_until_complete( asyncio.gather(mycor()))
loop.close()

